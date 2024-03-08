const UTF_8_LOOKUP: [u8; 16] = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 4 ];

fn get_utf8_char_len(b: u8) -> usize {
    UTF_8_LOOKUP[(b >> 4) as usize] as usize
}

pub(crate) fn valid_utf8_character_with_char_len(source: &[u8]) -> (Option<char>, usize) {
    if source.is_empty() {
        return (None, 0);
    }
    let first = source[0];
    let len = get_utf8_char_len(first);
    std::str::from_utf8(&source[0..len]).ok().map(|s| (Some(s.chars().next().unwrap()), len)).unwrap_or((None, 0))
}

pub(crate) fn escape_string(source: &str) -> String {
    let mut result = String::new();
    for ch in source.chars() {
        match ch {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            _ => result.push(ch),
        }
    }
    result
}

fn decode_unicode_escape_sequence_helper(source: &str) -> Option<char> {
    let mut result = 0;
    let mut i = 0;
    while i < source.len() {
        let ch = source.chars().nth(i)?;
        let digit = ch.to_digit(16).unwrap();
        result = result * 16 + digit;
        i += 1;
    }
    std::char::from_u32(result)
}

// "\u{090}"
pub(crate) fn decode_unicode_escape_sequence(source: &[u8]) -> String{
    let mut result = String::new();
    let temp_str = std::str::from_utf8(source).unwrap();
    let mut i = 0;
    let count = temp_str.chars().count();
    while i < count {
        let ch = temp_str.chars().nth(i).unwrap();
        let remaining = temp_str.len() - i;
        if ch == '\\' && remaining >= 3 {
            i += 1;
            let ch = temp_str.chars().nth(i).unwrap();
            if ch == 'u' {
                i += 1;
                let ch = temp_str.chars().nth(i).unwrap();
                if ch == '{' {
                    i += 1;
                    let start = i;
                    let mut end = i;
                    while i < temp_str.len() {
                        let ch = temp_str.chars().nth(i).unwrap();
                        if ch == '}' {
                            end = i;
                            i += 1;
                            break;
                        }
                        i += 1;
                    }
                    let slice = &temp_str[start..end];
                    if !slice.is_empty() {
                        if let Some(ch) = decode_unicode_escape_sequence_helper(slice) {
                            result.push(ch);
                            continue;
                        }
                    }
                }
            }
        }

        result.push(ch);
            i += 1;

    }

    result
}

pub(crate) fn byte_to_char(source: &[u8]) -> (Option<char>, usize) {
    if source.is_empty() {
        return (None, 0);
    }
    
    if source[0] < 128 {
        if source[0] == b'\r' {
            if 1 < source.len() && source[1] == b'\n' {
                return (Some('\n'), 2);
            }
            return (Some('\n'), 1);
        }
        return (Some(source[0] as char), 1);
    }

    valid_utf8_character_with_char_len(source)
}

pub(crate) struct ByteToCharIter<'a>(&'a [u8]);

impl<'a> ByteToCharIter<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        ByteToCharIter(bytes)
    }

    pub(crate) fn utf8_len_after_skip(&self, mut n: usize) -> usize {
        let mut size = 0usize;
        let mut iter = ByteToCharIter::new(self.0);
        while let Some(c) = iter.next() {
            if n == 0 {
                break;
            }
            size += c.len_utf8();
            n -= 1;
        }
        size
    }
}

impl<'a> Iterator for ByteToCharIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() {
            return None;
        }

        let (ch, size) = byte_to_char(self.0);
        self.0 = &self.0[size..];
        ch
    }
}
