use crate::eoc::utils::{
    diagnostic::{Diagnostic, DiagnosticLevel, DiagnosticReporter},
    span::Span,
};

use super::{ebnf::ast::RelativeSourceManager, str_utils::ByteToCharIter};

pub(crate) fn parse_integer<'b, D, H, O, B>(
    s: &'b [u8],
    get_digit: D,
    get_hex_digit: H,
    get_oct_digit: O,
    get_binary_digit: B,
    source_manager: RelativeSourceManager<'b>,
    diagnostic: &mut Diagnostic,
) -> Option<&'b [u8]>
where
    D: Fn(&'b [u8], RelativeSourceManager<'b>, &mut Diagnostic) -> Option<char>,
    H: Fn(&'b [u8], RelativeSourceManager<'b>, &mut Diagnostic) -> Option<char>,
    O: Fn(&'b [u8], RelativeSourceManager<'b>, &mut Diagnostic) -> Option<char>,
    B: Fn(&'b [u8], RelativeSourceManager<'b>, &mut Diagnostic) -> Option<char>,
{
    if s.is_empty() {
        return None;
    }

    let mut iter = ByteToCharIter::new(s).peekable();
    let mut end = 0usize;

    if iter.peek() == Some(&'.') {
        return None;
    }

    let mut has_dot = false;
    let has_hex = s.starts_with(b"0x") || s.starts_with(b"0X");

    let mut has_e = false;
    let mut has_p = false;

    while let Some(ch) = iter.peek() {
        has_dot = (*ch == '.') || has_dot;

        has_e = (*ch == 'e' || *ch == 'E') || has_e;
        has_p = (*ch == 'p' || *ch == 'P') || has_p;

        match *ch {
            ch if ch.is_whitespace() => break,
            _ => {
                iter.next();
            }
        }
    }

    if has_dot || (!has_hex && has_e) || has_p {
        return None;
    }

    let mut matched = s;

    let ch = get_digit(matched, source_manager, diagnostic);

    if ch.is_none() {
        return None;
    }

    let ch = ch.unwrap();

    let is_start_with_zero = ch == '0';

    end += ch.len_utf8();
    matched = &s[end..];

    if is_start_with_zero {
        let ch = ByteToCharIter::new(matched).next();
        let mut underscore_count = 0usize;
        if ch == Some('x') {
            end += 1;
            matched = &s[end..];
            let start = end;
            while let Some(ch) = ByteToCharIter::new(matched).next() {
                if let Some(ch) = get_hex_digit(matched, source_manager, diagnostic) {
                    end += ch.len_utf8();
                    matched = &s[end..];
                } else if ch == '_' {
                    end += ch.len_utf8();
                    matched = &s[end..];
                    underscore_count += 1;
                } else {
                    break;
                }
            }

            if start == (end - underscore_count) {
                diagnostic
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Expecting hexadecimal after '0x', but found none",
                        source_manager.get_source_info(Span::from_usize(start, start + 1)),
                        None,
                    )
                    .add_error(
                        "Hex digit must be between 0 and 9 or a and f",
                        Some(source_manager.fix_span(Span::from_usize(start, end))),
                    )
                    .commit();
            }
            return Some(&s[..end]);
        }

        if ch == Some('o') {
            end += 1;

            matched = &s[end..];
            let start = end;
            while let Some(ch) = ByteToCharIter::new(matched).next() {
                if let Some(ch) = get_oct_digit(matched, source_manager, diagnostic) {
                    end += ch.len_utf8();
                    matched = &s[end..];
                } else if ch == '_' {
                    end += ch.len_utf8();
                    matched = &s[end..];
                    underscore_count += 1;
                } else {
                    break;
                }
            }

            if start == (end - underscore_count) {
                diagnostic
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Expecting octal after '0o', but found none",
                        source_manager.get_source_info(Span::from_usize(start, start + 1)),
                        None,
                    )
                    .add_error(
                        "Octal digit must be between 0 and 7",
                        Some(source_manager.fix_span(Span::from_usize(start, end))),
                    )
                    .commit();
            }

            return Some(&s[..end]);
        }

        if ch == Some('b') {
            end += 1;
            matched = &s[end..];

            let start = end;

            while let Some(ch) = ByteToCharIter::new(matched).next() {
                if let Some(ch) = get_binary_digit(matched, source_manager, diagnostic) {
                    end += ch.len_utf8();
                    matched = &s[end..];
                } else if ch == '_' {
                    end += ch.len_utf8();
                    matched = &s[end..];
                    underscore_count += 1;
                } else {
                    break;
                }
            }

            if start == (end - underscore_count) {
                diagnostic
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Expecting binary after '0b', but found none",
                        source_manager.get_source_info(Span::from_usize(start, start + 1)),
                        None,
                    )
                    .add_error(
                        "Binary digit must be 0 or 1",
                        Some(source_manager.fix_span(Span::from_usize(start, end))),
                    )
                    .commit();
            }
            return Some(&s[..end]);
        }
    }

    while let Some(ch) = ByteToCharIter::new(matched).next() {
        if let Some(ch) = get_digit(matched, source_manager, diagnostic) {
            end += ch.len_utf8();
            matched = &s[end..];
        } else if ch == '_' {
            end += ch.len_utf8();
            matched = &s[end..];
        } else {
            break;
        }
    }

    if end == 0 {
        return None;
    }

    Some(&s[..end])
}

pub(crate) fn parse_floating_point<'b, D, H>(
    s: &'b [u8],
    get_digit: D,
    get_hex_digit: H,
    source_manager: RelativeSourceManager<'b>,
    diagnostic: &mut Diagnostic,
) -> Option<&'b [u8]>
where
    D: Fn(&'b [u8], RelativeSourceManager<'b>, &mut Diagnostic) -> Option<char>,
    H: Fn(&'b [u8], RelativeSourceManager<'b>, &mut Diagnostic) -> Option<char>,
{
    if s.is_empty() {
        return None;
    }

    let mut iter = ByteToCharIter::new(s).peekable();
    let mut end = 0usize;
    let mut matched = s;

    let ch = iter.peek();
    if ch.is_none() {
        return None;
    }

    let ch = unsafe { *ch.unwrap_unchecked() };

    let ch = match (get_digit(matched, source_manager, diagnostic), ch) {
        (_, '.') => Some('.'),
        (Some(ch), _) => Some(ch),
        _ => None,
    };

    if ch.is_none() {
        return None;
    }

    let ch = unsafe { ch.unwrap_unchecked() };

    let is_start_with_zero = ch == '0';
    end += ch.len_utf8();

    matched = &s[end..];

    if is_start_with_zero && (get_digit(matched, source_manager, diagnostic) == Some('x')) {
        end += 1;
        let start = end;
        let mut underscore_count = 0usize;
        while let Some(ch) = ByteToCharIter::new(matched).next() {
            if let Some(ch) = get_hex_digit(matched, source_manager, diagnostic) {
                end += ch.len_utf8();
                matched = &s[end..];
            } else if ch == '_' {
                end += ch.len_utf8();
                matched = &s[end..];
                underscore_count += 1;
            } else {
                break;
            }
        }

        if start == (end - underscore_count) {
            diagnostic
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    "Expecting hexadecimal after '0x', but found none",
                    source_manager.get_source_info(Span::from_usize(start, start + 1)),
                    None,
                )
                .add_error(
                    "Hex digit must be between 0 and 9 or a and f",
                    Some(source_manager.fix_span(Span::from_usize(start, end))),
                )
                .commit();
            return None;
        }

        matched = &s[end..];

        if get_digit(matched, source_manager, diagnostic) == Some('.') {
            end += 1;
            matched = &s[end..];
            while let Some(ch) = ByteToCharIter::new(matched).next() {
                if let Some(ch) = get_hex_digit(matched, source_manager, diagnostic) {
                    end += ch.len_utf8();
                    matched = &s[end..];
                } else if ch == '_' {
                    end += ch.len_utf8();
                    matched = &s[end..];
                    underscore_count += 1;
                } else {
                    break;
                }
            }
        }

        let ch = ByteToCharIter::new(matched).next();
        if ch == Some('p') || ch == Some('P') {
            end += 1;
            matched = &s[end..];
            let ch = ByteToCharIter::new(matched).next();
            if ch == Some('+') || ch == Some('-') {
                end += 1;
                matched = &s[end..];
            }
            while let Some(ch) = ByteToCharIter::new(matched).next() {
                if let Some(ch) = get_hex_digit(matched, source_manager, diagnostic) {
                    end += ch.len_utf8();
                    matched = &s[end..];
                } else if ch == '_' {
                    end += ch.len_utf8();
                    matched = &s[end..];
                    underscore_count += 1;
                } else {
                    break;
                }
            }
        }

        return Some(&s[..end]);
    }

    while let Some(ch) = ByteToCharIter::new(matched).next() {
        if let Some(ch) = get_digit(matched, source_manager, diagnostic) {
            end += ch.len_utf8();
            matched = &s[end..];
        } else if ch == '_' {
            end += ch.len_utf8();
            matched = &s[end..];
        } else {
            break;
        }
    }

    matched = &s[end..];
    if ByteToCharIter::new(matched).next() == Some('.') {
        end += 1;
        matched = &s[end..];
        while let Some(ch) = ByteToCharIter::new(matched).next() {
            if let Some(ch) = get_digit(matched, source_manager, diagnostic) {
                end += ch.len_utf8();
                matched = &s[end..];
            } else if ch == '_' {
                end += ch.len_utf8();
                matched = &s[end..];
            } else {
                break;
            }
        }
    }

    let ch = ByteToCharIter::new(matched).next();
    if ch == Some('e') || ch == Some('E') {
        end += 1;
        matched = &s[end..];
        let ch = ByteToCharIter::new(matched).next();
        if ch == Some('+') || ch == Some('-') {
            end += 1;
            matched = &s[end..];
        }
        while let Some(ch) = ByteToCharIter::new(matched).next() {
            if let Some(ch) = get_digit(matched, source_manager, diagnostic) {
                end += ch.len_utf8();
                matched = &s[end..];
            } else if ch == '_' {
                end += ch.len_utf8();
                matched = &s[end..];
            } else {
                break;
            }
        }
    }

    if end == 0 {
        return None;
    }

    Some(&s[..end])
}
