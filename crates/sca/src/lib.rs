use regex::Regex;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
pub struct SoundChangeApplier {
    categories: Categories,
    rewrite_rules: Vec<RewriteRule>,
    rules: Vec<RuleEntry>,
}

#[derive(Debug, Clone)]
struct Categories {
    entries: HashMap<char, Vec<char>>,
    order: Vec<char>,
}

#[derive(Debug, Clone)]
struct RewriteRule {
    regex: Regex,
    replacement: String,
}

#[derive(Debug, Clone)]
enum RuleEntry {
    Marker,
    Rule(SoundRule),
}

#[derive(Debug, Clone)]
struct SoundRule {
    target: Vec<char>,
    replacement: Vec<char>,
    is_reverse: bool,
    environment: Vec<char>,
    exception: Option<Vec<char>>,
}

#[derive(Debug)]
pub enum SoundChangeError {
    InvalidCategoryFormat(String),
    DuplicateCategory(char),
    InvalidRuleFormat(String),
    InvalidRegex(regex::Error),
}

impl From<regex::Error> for SoundChangeError {
    fn from(err: regex::Error) -> Self {
        SoundChangeError::InvalidRegex(err)
    }
}

impl fmt::Display for SoundChangeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SoundChangeError::InvalidCategoryFormat(line) => {
                write!(f, "invalid category definition: {line}")
            }
            SoundChangeError::DuplicateCategory(symbol) => {
                write!(f, "duplicate category symbol: {symbol}")
            }
            SoundChangeError::InvalidRuleFormat(line) => {
                write!(f, "invalid sound change rule: {line}")
            }
            SoundChangeError::InvalidRegex(err) => write!(f, "invalid rewrite regex: {err}"),
        }
    }
}

impl std::error::Error for SoundChangeError {}

impl SoundChangeApplier {
    pub fn new(
        categories_src: &str,
        rewrite_src: &str,
        rules_src: &str,
    ) -> Result<Self, SoundChangeError> {
        let rewrite_rules = parse_rewrite_rules(rewrite_src)?;
        let categories = parse_categories(categories_src, &rewrite_rules)?;
        let rules = parse_rules(rules_src, &rewrite_rules)?;

        if !rules
            .iter()
            .any(|entry| matches!(entry, RuleEntry::Rule(_)))
        {
            return Err(SoundChangeError::InvalidRuleFormat(
                "no valid sound change rules found".to_string(),
            ));
        }

        Ok(Self {
            categories,
            rewrite_rules,
            rules,
        })
    }

    pub fn apply_lexicon(&self, lexicon_src: &str) -> ProcessedLexicon {
        let rewritten = apply_rewrites(lexicon_src, &self.rewrite_rules);
        let mut outputs = Vec::new();
        let mut words_processed = 0usize;
        let mut words_changed = 0usize;

        for raw_line in rewritten.lines() {
            let line = raw_line.trim_end_matches('\r').trim_end();
            if line.is_empty() {
                continue;
            }
            words_processed += 1;

            let (word, gloss) = split_gloss(line);
            let transformed = self.transform_word(word);
            if transformed != word {
                words_changed += 1;
            }
            outputs.push(WordOutput {
                original: word.to_string(),
                transformed,
                gloss: gloss.map(|g| g.to_string()),
            });
        }

        ProcessedLexicon {
            entries: outputs,
            words_processed,
            words_changed,
        }
    }

    fn transform_word(&self, input: &str) -> String {
        let mut current = input.to_string();
        for entry in &self.rules {
            match entry {
                RuleEntry::Marker => {}
                RuleEntry::Rule(rule) => {
                    current = self.apply_rule(&current, rule);
                }
            }
        }
        current
    }

    pub fn category_symbols(&self) -> &[char] {
        self.categories.order()
    }

    pub fn rule_count(&self) -> usize {
        self.rules
            .iter()
            .filter(|entry| matches!(entry, RuleEntry::Rule(_)))
            .count()
    }

    fn apply_rule(&self, word: &str, rule: &SoundRule) -> String {
        let mut chars: Vec<char> = word.chars().collect();
        let mut i = 0usize;

        loop {
            let limit = chars.iter().position(|&c| c == '‣').unwrap_or(chars.len());

            if i > limit {
                break;
            }

            if let Some(state) = self.match_environment(&chars, i, rule) {
                if self.exception_applies(&chars, &state, rule) {
                    i += 1;
                    continue;
                }
                let replacement = self.build_replacement(&chars, &state, rule);
                let end = state.start + state.length;
                chars.splice(state.start..end, replacement.iter().cloned());
                let inserted = replacement.len();
                let advance_base = state.start + inserted;
                if state.length == 0 {
                    i = advance_base + 1;
                } else {
                    i = advance_base;
                }
                continue;
            }

            if i == limit {
                break;
            }
            i += 1;
        }

        chars.into_iter().collect()
    }

    fn match_environment(
        &self,
        word: &[char],
        index: usize,
        rule: &SoundRule,
    ) -> Option<MatchState> {
        match_environment(
            word,
            index,
            &rule.target,
            &rule.environment,
            &self.categories,
        )
    }

    fn exception_applies(&self, word: &[char], state: &MatchState, rule: &SoundRule) -> bool {
        match &rule.exception {
            None => false,
            Some(env) => exception_matches(word, state, &rule.target, env, &self.categories),
        }
    }

    fn build_replacement(&self, word: &[char], state: &MatchState, rule: &SoundRule) -> Vec<char> {
        if rule.is_reverse {
            let slice = &word[state.start..state.start + state.length];
            let reversed: Vec<char> = slice.iter().rev().copied().collect();
            return reversed;
        }

        if let Some(idx) = state.category_index {
            category_substitute(&rule.replacement, idx, &self.categories)
        } else {
            rule.replacement.clone()
        }
    }
}

impl Categories {
    fn expansions(&self, symbol: char) -> Option<&[char]> {
        self.entries.get(&symbol).map(Vec::as_slice)
    }

    fn contains(&self, symbol: char, ch: char) -> bool {
        self.entries
            .get(&symbol)
            .map_or(false, |expansions| expansions.contains(&ch))
    }

    fn position(&self, symbol: char, ch: char) -> Option<usize> {
        self.entries
            .get(&symbol)
            .and_then(|expansions| expansions.iter().position(|&item| item == ch))
    }

    fn order(&self) -> &[char] {
        &self.order
    }
}

#[derive(Debug)]
pub struct ProcessedLexicon {
    pub entries: Vec<WordOutput>,
    pub words_processed: usize,
    pub words_changed: usize,
}

#[derive(Debug)]
pub struct WordOutput {
    pub original: String,
    pub transformed: String,
    pub gloss: Option<String>,
}

#[derive(Debug, Clone)]
struct MatchState {
    start: usize,
    length: usize,
    category_index: Option<usize>,
}

impl MatchState {
    fn new(start: usize, length: usize, category_index: Option<usize>) -> Self {
        Self {
            start,
            length,
            category_index,
        }
    }
}

fn parse_rewrite_rules(src: &str) -> Result<Vec<RewriteRule>, SoundChangeError> {
    let mut rules = Vec::new();
    for line in src.lines() {
        let text = line.trim_end_matches('\r');
        if text.len() > 2 {
            if let Some(idx) = text.find('|') {
                let pattern = &text[..idx];
                let replacement = &text[idx + 1..];
                let regex = Regex::new(pattern)?;
                rules.push(RewriteRule {
                    regex,
                    replacement: replacement.to_string(),
                });
            }
        }
    }
    Ok(rules)
}

fn apply_rewrites(text: &str, rules: &[RewriteRule]) -> String {
    let mut current = text.to_string();
    for rule in rules {
        current = rule
            .regex
            .replace_all(&current, rule.replacement.as_str())
            .into_owned();
    }
    current
}

fn parse_categories(src: &str, rewrites: &[RewriteRule]) -> Result<Categories, SoundChangeError> {
    let rewritten = apply_rewrites(src, rewrites);
    let mut entries = HashMap::new();
    let mut order = Vec::new();
    for raw_line in rewritten.lines() {
        let line = raw_line.trim_end_matches('\r');
        if line.is_empty() {
            continue;
        }
        let mut parts = line.splitn(2, '=');
        let key = parts
            .next()
            .ok_or_else(|| SoundChangeError::InvalidCategoryFormat(line.to_string()))?;
        let value = parts
            .next()
            .ok_or_else(|| SoundChangeError::InvalidCategoryFormat(line.to_string()))?;
        if key.chars().count() != 1 {
            return Err(SoundChangeError::InvalidCategoryFormat(line.to_string()));
        }
        let symbol = key.chars().next().unwrap();
        if entries.contains_key(&symbol) {
            return Err(SoundChangeError::DuplicateCategory(symbol));
        }
        let chars = value.chars().collect::<Vec<char>>();
        entries.insert(symbol, chars);
        order.push(symbol);
    }
    Ok(Categories { entries, order })
}

fn parse_rules(src: &str, rewrites: &[RewriteRule]) -> Result<Vec<RuleEntry>, SoundChangeError> {
    let rewritten = apply_rewrites(src, rewrites);
    let mut rules = Vec::new();
    for raw_line in rewritten.lines() {
        let line = raw_line.trim_end_matches('\r');
        if line.is_empty() {
            continue;
        }
        if line.starts_with("-*") {
            rules.push(RuleEntry::Marker);
            continue;
        }

        if !line.contains('_') {
            continue;
        }

        let parts = line.split('/').collect::<Vec<_>>();
        let mut valid = parts.len() > 2 || (parts.len() == 2 && parts[0].contains('→'));
        if valid && parts[0].is_empty() {
            valid = parts.len() > 2 && !parts[1].is_empty() && parts[2] != "_";
        }
        if !valid {
            continue;
        }

        let normalized = line.replace('→', "/");
        let segments = normalized.split('/').collect::<Vec<_>>();
        if segments.len() < 3 {
            continue;
        }

        let target = segments[0].chars().collect::<Vec<char>>();
        let replacement_raw = segments[1].to_string();
        let replacement = replacement_raw.chars().collect::<Vec<char>>();
        let environment = segments[2].chars().collect::<Vec<char>>();
        let exception = if segments.len() > 3 && !segments[3].is_empty() {
            Some(segments[3].chars().collect::<Vec<char>>())
        } else {
            None
        };

        let is_reverse = replacement_raw == "\\\\";

        let rule = SoundRule {
            target,
            replacement,
            is_reverse,
            environment,
            exception,
        };
        rules.push(RuleEntry::Rule(rule));
    }
    Ok(rules)
}

fn split_gloss(line: &str) -> (&str, Option<&str>) {
    if let Some(idx) = line.find(" ‣") {
        let (word, rest) = line.split_at(idx);
        let gloss = rest.trim_start_matches(" ‣");
        if gloss.is_empty() {
            (word, None)
        } else {
            (word, Some(gloss))
        }
    } else {
        (line, None)
    }
}

fn match_environment(
    word: &[char],
    start_index: usize,
    target: &[char],
    environment: &[char],
    categories: &Categories,
) -> Option<MatchState> {
    let mut state = InternalMatchState::new();
    if match_env_recursive(
        word,
        start_index,
        target,
        environment,
        categories,
        &mut state,
    ) {
        if let Some(target_start) = state.target_start {
            return Some(MatchState::new(
                target_start,
                state.target_length,
                state.category_index,
            ));
        }
    }
    None
}

#[derive(Clone)]
struct InternalMatchState {
    target_start: Option<usize>,
    target_length: usize,
    category_index: Option<usize>,
}

impl InternalMatchState {
    fn new() -> Self {
        Self {
            target_start: None,
            target_length: 0,
            category_index: None,
        }
    }
}

fn match_env_recursive(
    word: &[char],
    mut index: usize,
    target: &[char],
    environment: &[char],
    categories: &Categories,
    state: &mut InternalMatchState,
) -> bool {
    let mut optional = false;
    let mut j = 0usize;

    while j < environment.len() {
        let symbol = environment[j];
        match symbol {
            '[' => {
                j += 1;
                let mut found = false;
                while j < environment.len() && environment[j] != ']' {
                    if found {
                        j += 1;
                        continue;
                    }
                    let candidate = environment[j];
                    if candidate == '#' {
                        if at_space(word, index, state.target_start) {
                            found = true;
                        }
                    } else if index < word.len() && categories.contains(candidate, word[index]) {
                        found = true;
                        index += 1;
                    } else if index < word.len() && word[index] == candidate {
                        found = true;
                        index += 1;
                    }
                    j += 1;
                }
                if !found && !optional {
                    return false;
                }
                // Skip closing bracket if present
                if j < environment.len() && environment[j] == ']' {
                    j += 1;
                }
                continue;
            }
            '(' => {
                optional = true;
                j += 1;
                continue;
            }
            ')' => {
                optional = false;
                j += 1;
                continue;
            }
            '#' => {
                if !at_space(word, index, state.target_start) {
                    if optional {
                        j += 1;
                        continue;
                    }
                    return false;
                }
                j += 1;
                continue;
            }
            '²' => {
                if index == 0 || index >= word.len() || word[index] != word[index - 1] {
                    if optional {
                        j += 1;
                        continue;
                    }
                    return false;
                }
                index += 1;
                j += 1;
                continue;
            }
            '…' => {
                let remaining = environment[j + 1..].to_vec();
                for k in index..=word.len() {
                    if k < word.len() && word[k] == ' ' {
                        break;
                    }
                    let mut nested_state = state.clone();
                    if match_env_recursive(
                        word,
                        k,
                        target,
                        &remaining,
                        categories,
                        &mut nested_state,
                    ) {
                        *state = nested_state;
                        return true;
                    }
                }
                return false;
            }
            '_' => {
                state.target_start = Some(index);
                if target.is_empty() {
                    state.target_length = 0;
                    j += 1;
                    continue;
                }
                if let Some((len, cat_idx)) = match_target(word, index, target, categories) {
                    state.target_length = len;
                    state.category_index = cat_idx;
                    index += len;
                    j += 1;
                    continue;
                }
                return false;
            }
            _ => {
                if index >= word.len() {
                    if optional {
                        j += 1;
                        continue;
                    }
                    return false;
                }
                if match_char_or_category(word[index], symbol, categories) {
                    index += 1;
                    j += 1;
                    continue;
                }
                if optional {
                    j += 1;
                    continue;
                }
                return false;
            }
        }
    }

    true
}

fn at_space(word: &[char], index: usize, target_start: Option<usize>) -> bool {
    if target_start.is_none() {
        if index == 0 {
            return true;
        }
        if index > 0 && word.get(index - 1) == Some(&' ') {
            return true;
        }
    } else {
        if index >= word.len() {
            return true;
        }
        if word.get(index) == Some(&' ') {
            return true;
        }
    }
    false
}

fn match_char_or_category(ch: char, symbol: char, categories: &Categories) -> bool {
    if categories.contains(symbol, ch) {
        true
    } else {
        ch == symbol
    }
}

fn match_target(
    word: &[char],
    index: usize,
    target: &[char],
    categories: &Categories,
) -> Option<(usize, Option<usize>)> {
    if target.is_empty() {
        return Some((0, None));
    }

    let first = target[0];
    if index < word.len() {
        if let Some(pos) = categories.position(first, word[index]) {
            if target.len() == 1 {
                return Some((1, Some(pos)));
            }
            let len = match_target_literal(word, index + 1, &target[1..], categories)?;
            return Some((len + 1, Some(pos)));
        }
    }

    let len = match_target_literal(word, index, target, categories)?;
    Some((len, None))
}

fn match_target_literal(
    word: &[char],
    index: usize,
    target: &[char],
    categories: &Categories,
) -> Option<usize> {
    if target.iter().any(|&c| c == '[') {
        let mut i = index;
        let mut glen = 0usize;
        let mut in_bracket = false;
        let mut found_inside = false;
        let mut j = 0usize;
        while j < target.len() {
            let symbol = target[j];
            match symbol {
                '[' => {
                    in_bracket = true;
                    found_inside = false;
                }
                ']' => {
                    if !found_inside {
                        return None;
                    }
                    i += 1;
                    glen += 1;
                    in_bracket = false;
                }
                _ if in_bracket => {
                    if i >= word.len() {
                        return None;
                    }
                    if !found_inside && word[i] == symbol {
                        found_inside = true;
                    }
                }
                _ => {
                    if i >= word.len() {
                        return None;
                    }
                    if !match_char_or_category(word[i], symbol, categories) {
                        return None;
                    }
                    i += 1;
                    glen += 1;
                }
            }
            j += 1;
        }
        Some(glen)
    } else {
        for (offset, &symbol) in target.iter().enumerate() {
            if index + offset >= word.len() {
                return None;
            }
            if !match_char_or_category(word[index + offset], symbol, categories) {
                return None;
            }
        }
        Some(target.len())
    }
}

fn exception_matches(
    word: &[char],
    state: &MatchState,
    target: &[char],
    exception_env: &[char],
    categories: &Categories,
) -> bool {
    let Some(pos) = exception_env.iter().position(|&c| c == '_') else {
        return false;
    };
    let precount = count_preceding_chars(exception_env, pos);
    if state.start < precount {
        return false;
    }
    let start = state.start - precount;
    match_environment(word, start, target, exception_env, categories).is_some()
}

fn count_preceding_chars(env: &[char], underscore_index: usize) -> usize {
    let mut count = 0usize;
    let mut brackets = false;
    for &symbol in &env[..underscore_index] {
        match symbol {
            '[' => brackets = true,
            ']' => {
                brackets = false;
                count += 1;
            }
            '#' => {}
            _ => {
                if !brackets {
                    count += 1;
                }
            }
        }
    }
    count
}

fn category_substitute(
    replacement: &[char],
    category_index: usize,
    categories: &Categories,
) -> Vec<char> {
    let mut result = Vec::new();
    let mut last: Option<char> = None;
    for &symbol in replacement {
        if let Some(expansions) = categories.expansions(symbol) {
            if let Some(&ch) = expansions.get(category_index) {
                result.push(ch);
                last = Some(ch);
            }
        } else if symbol == '²' {
            if let Some(ch) = last {
                result.push(ch);
            }
        } else {
            result.push(symbol);
            last = Some(symbol);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_rule_application() {
        let categories = "V=aeiou\n";
        let rewrites = "";
        let rules = "p/f/_V";
        let applier = SoundChangeApplier::new(categories, rewrites, rules).unwrap();
        let result = applier.apply_lexicon("pa\npe\npi");
        let words: Vec<_> = result
            .entries
            .iter()
            .map(|w| w.transformed.as_str())
            .collect();
        assert_eq!(words, vec!["fa", "fe", "fi"]);
    }
}
