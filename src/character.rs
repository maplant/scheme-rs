use crate::{exception::Exception, gc::Gc, num::Number, registry::bridge, value::Value};
use std::slice;
use unicode_categories::UnicodeCategories;

fn digit_to_num(ch: char) -> Option<i64> {
    macro_rules! convert_digits_to_num {
        ($char:expr, [
            $($start:literal..$end:literal),* $(,)?
        ]) => {
            match $char {
                $($start..=$end => Some($char as i64 - $start as i64),)*
                _ => None,
            }
        }
    }

    // i love magic numbers
    // https://www.unicode.org/Public/UNIDATA/extracted/DerivedNumericType.txt
    convert_digits_to_num!(
        ch,
        [
            '\u{0030}'..'\u{0039}',   // DIGIT ZERO..DIGIT NINE
            '\u{0660}'..'\u{0669}',   // ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
            '\u{06F0}'..'\u{06F9}', // EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE
            '\u{07C0}'..'\u{07C9}', // NKO DIGIT ZERO..NKO DIGIT NINE
            '\u{0966}'..'\u{096F}', // DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE
            '\u{09E6}'..'\u{09EF}', // BENGALI DIGIT ZERO..BENGALI DIGIT NINE
            '\u{0A66}'..'\u{0A6F}', // GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE
            '\u{0AE6}'..'\u{0AEF}', // GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE
            '\u{0B66}'..'\u{0B6F}', // ORIYA DIGIT ZERO..ORIYA DIGIT NINE
            '\u{0BE6}'..'\u{0BEF}', // TAMIL DIGIT ZERO..TAMIL DIGIT NINE
            '\u{0C66}'..'\u{0C6F}', // TELUGU DIGIT ZERO..TELUGU DIGIT NINE
            '\u{0CE6}'..'\u{0CEF}', // KANNADA DIGIT ZERO..KANNADA DIGIT NINE
            '\u{0D66}'..'\u{0D6F}', // MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE
            '\u{0DE6}'..'\u{0DEF}', // SINHALA LITH DIGIT ZERO..SINHALA LITH DIGIT NINE
            '\u{0E50}'..'\u{0E59}', // THAI DIGIT ZERO..THAI DIGIT NINE
            '\u{0ED0}'..'\u{0ED9}', // LAO DIGIT ZERO..LAO DIGIT NINE
            '\u{0F20}'..'\u{0F29}', // TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE
            '\u{1040}'..'\u{1049}', // MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE
            '\u{1090}'..'\u{1099}', // MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE
            '\u{17E0}'..'\u{17E9}', // KHMER DIGIT ZERO..KHMER DIGIT NINE
            '\u{1810}'..'\u{1819}', // MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE
            '\u{1946}'..'\u{194F}', // LIMBU DIGIT ZERO..LIMBU DIGIT NINE
            '\u{19D0}'..'\u{19D9}', // NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE
            '\u{1A80}'..'\u{1A89}', // TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE
            '\u{1A90}'..'\u{1A99}', // TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE
            '\u{1B50}'..'\u{1B59}', // BALINESE DIGIT ZERO..BALINESE DIGIT NINE
            '\u{1BB0}'..'\u{1BB9}', // SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE
            '\u{1C40}'..'\u{1C49}', // LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE
            '\u{1C50}'..'\u{1C59}', // OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE
            '\u{A620}'..'\u{A629}', // VAI DIGIT ZERO..VAI DIGIT NINE
            '\u{A8D0}'..'\u{A8D9}', // SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE
            '\u{A900}'..'\u{A909}', // KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE
            '\u{A9D0}'..'\u{A9D9}', // JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE
            '\u{A9F0}'..'\u{A9F9}', // MYANMAR TAI LAING DIGIT ZERO..MYANMAR TAI LAING DIGIT NINE
            '\u{AA50}'..'\u{AA59}', // CHAM DIGIT ZERO..CHAM DIGIT NINE
            '\u{ABF0}'..'\u{ABF9}', // MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE
            '\u{FF10}'..'\u{FF19}', // FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
            '\u{104A0}'..'\u{104A9}', // OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE
            '\u{10D30}'..'\u{10D39}', // HANIFI ROHINGYA DIGIT ZERO..HANIFI ROHINGYA DIGIT NINE
            '\u{10D40}'..'\u{10D49}', // GARAY DIGIT ZERO..GARAY DIGIT NINE
            '\u{11066}'..'\u{1106F}', // BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE
            '\u{110F0}'..'\u{110F9}', // SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE
            '\u{11136}'..'\u{1113F}', // CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE
            '\u{111D0}'..'\u{111D9}', // SHARADA DIGIT ZERO..SHARADA DIGIT NINE
            '\u{112F0}'..'\u{112F9}', // KHUDAWADI DIGIT ZERO..KHUDAWADI DIGIT NINE
            '\u{11450}'..'\u{11459}', // NEWA DIGIT ZERO..NEWA DIGIT NINE
            '\u{114D0}'..'\u{114D9}', // TIRHUTA DIGIT ZERO..TIRHUTA DIGIT NINE
            '\u{11650}'..'\u{11659}', // MODI DIGIT ZERO..MODI DIGIT NINE
            '\u{116C0}'..'\u{116C9}', // TAKRI DIGIT ZERO..TAKRI DIGIT NINE
            '\u{116D0}'..'\u{116d9}', // MYANMAR PAO DIGIT ZERO..MYANMAR EASTERN PWO KAREN DIGIT NINE
            '\u{116da}'..'\u{116e3}', // MYANMAR PAO DIGIT ZERO..MYANMAR EASTERN PWO KAREN DIGIT NINE
            '\u{11730}'..'\u{11739}', // AHOM DIGIT ZERO..AHOM DIGIT NINE
            '\u{118E0}'..'\u{118E9}', // WARANG CITI DIGIT ZERO..WARANG CITI DIGIT NINE
            '\u{11950}'..'\u{11959}', // DIVES AKURU DIGIT ZERO..DIVES AKURU DIGIT NINE
            '\u{11BF0}'..'\u{11BF9}', // SUNUWAR DIGIT ZERO..SUNUWAR DIGIT NINE
            '\u{11C50}'..'\u{11C59}', // BHAIKSUKI DIGIT ZERO..BHAIKSUKI DIGIT NINE
            '\u{11D50}'..'\u{11D59}', // MASARAM GONDI DIGIT ZERO..MASARAM GONDI DIGIT NINE
            '\u{11DA0}'..'\u{11DA9}', // GUNJALA GONDI DIGIT ZERO..GUNJALA GONDI DIGIT NINE
            '\u{11F50}'..'\u{11F59}', // KAWI DIGIT ZERO..KAWI DIGIT NINE
            '\u{16130}'..'\u{16139}', // GURUNG KHEMA DIGIT ZERO..GURUNG KHEMA DIGIT NINE
            '\u{16A60}'..'\u{16A69}', // MRO DIGIT ZERO..MRO DIGIT NINE
            '\u{16AC0}'..'\u{16AC9}', // TANGSA DIGIT ZERO..TANGSA DIGIT NINE
            '\u{16B50}'..'\u{16B59}', // PAHAWH HMONG DIGIT ZERO..PAHAWH HMONG DIGIT NINE
            '\u{16D70}'..'\u{16D79}', // KIRAT RAI DIGIT ZERO..KIRAT RAI DIGIT NINE
            '\u{1CCF0}'..'\u{1CCF9}', // OUTLINED DIGIT ZERO..OUTLINED DIGIT NINE
            '\u{1D7CE}'..'\u{1D7D7}', // MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
            '\u{1D7D8}'..'\u{1D7E1}', // MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
            '\u{1D7E2}'..'\u{1D7EB}', // MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
            '\u{1D7EC}'..'\u{1D7F5}', // MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
            '\u{1D7F6}'..'\u{1D7FF}', // MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
            '\u{1E140}'..'\u{1E149}', // NYIAKENG PUACHUE HMONG DIGIT ZERO..NYIAKENG PUACHUE HMONG DIGIT NINE
            '\u{1E2F0}'..'\u{1E2F9}', // WANCHO DIGIT ZERO..WANCHO DIGIT NINE
            '\u{1E4F0}'..'\u{1E4F9}', // NAG MUNDARI DIGIT ZERO..NAG MUNDARI DIGIT NINE
            '\u{1E5F1}'..'\u{1E5FA}', // OL ONAL DIGIT ZERO..OL ONAL DIGIT NINE
            '\u{1E950}'..'\u{1E959}', // ADLAM DIGIT ZERO..ADLAM DIGIT NINE
            '\u{1FBF0}'..'\u{1FBF9}', // SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
        ]
    )
}

struct ToFoldcase<'a> {
    chars: &'a [char],
    i: u8,
}
impl<'a> ToFoldcase<'a> {
    pub const fn new(chars: &'a [char]) -> Self {
        Self { chars, i: 0 }
    }
}
impl Iterator for ToFoldcase<'_> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        let c = self.chars.get::<usize>(self.i.into());
        self.i = self.i.saturating_add(1);
        c.copied()
    }
}
impl ExactSizeIterator for ToFoldcase<'_> {
    fn len(&self) -> usize {
        self.chars.len().saturating_sub(self.i.into())
    }
}
const fn to_foldcase(ch: &char) -> ToFoldcase<'_> {
    // https://www.unicode.org/Public/UNIDATA/CaseFolding.txt
    ToFoldcase::new(match *ch {
        '\u{0041}' => &['\u{0061}'] as &[char], // LATIN CAPITAL LETTER A
        '\u{0042}' => &['\u{0062}'],            // LATIN CAPITAL LETTER B
        '\u{0043}' => &['\u{0063}'],            // LATIN CAPITAL LETTER C
        '\u{0044}' => &['\u{0064}'],            // LATIN CAPITAL LETTER D
        '\u{0045}' => &['\u{0065}'],            // LATIN CAPITAL LETTER E
        '\u{0046}' => &['\u{0066}'],            // LATIN CAPITAL LETTER F
        '\u{0047}' => &['\u{0067}'],            // LATIN CAPITAL LETTER G
        '\u{0048}' => &['\u{0068}'],            // LATIN CAPITAL LETTER H
        '\u{0049}' => &['\u{0069}'],            // LATIN CAPITAL LETTER I
        // '\u{0049}' => &['\u{0131}'],             // LATIN CAPITAL LETTER I
        '\u{004A}' => &['\u{006A}'], // LATIN CAPITAL LETTER J
        '\u{004B}' => &['\u{006B}'], // LATIN CAPITAL LETTER K
        '\u{004C}' => &['\u{006C}'], // LATIN CAPITAL LETTER L
        '\u{004D}' => &['\u{006D}'], // LATIN CAPITAL LETTER M
        '\u{004E}' => &['\u{006E}'], // LATIN CAPITAL LETTER N
        '\u{004F}' => &['\u{006F}'], // LATIN CAPITAL LETTER O
        '\u{0050}' => &['\u{0070}'], // LATIN CAPITAL LETTER P
        '\u{0051}' => &['\u{0071}'], // LATIN CAPITAL LETTER Q
        '\u{0052}' => &['\u{0072}'], // LATIN CAPITAL LETTER R
        '\u{0053}' => &['\u{0073}'], // LATIN CAPITAL LETTER S
        '\u{0054}' => &['\u{0074}'], // LATIN CAPITAL LETTER T
        '\u{0055}' => &['\u{0075}'], // LATIN CAPITAL LETTER U
        '\u{0056}' => &['\u{0076}'], // LATIN CAPITAL LETTER V
        '\u{0057}' => &['\u{0077}'], // LATIN CAPITAL LETTER W
        '\u{0058}' => &['\u{0078}'], // LATIN CAPITAL LETTER X
        '\u{0059}' => &['\u{0079}'], // LATIN CAPITAL LETTER Y
        '\u{005A}' => &['\u{007A}'], // LATIN CAPITAL LETTER Z
        '\u{00B5}' => &['\u{03BC}'], // MICRO SIGN
        '\u{00C0}' => &['\u{00E0}'], // LATIN CAPITAL LETTER A WITH GRAVE
        '\u{00C1}' => &['\u{00E1}'], // LATIN CAPITAL LETTER A WITH ACUTE
        '\u{00C2}' => &['\u{00E2}'], // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
        '\u{00C3}' => &['\u{00E3}'], // LATIN CAPITAL LETTER A WITH TILDE
        '\u{00C4}' => &['\u{00E4}'], // LATIN CAPITAL LETTER A WITH DIAERESIS
        '\u{00C5}' => &['\u{00E5}'], // LATIN CAPITAL LETTER A WITH RING ABOVE
        '\u{00C6}' => &['\u{00E6}'], // LATIN CAPITAL LETTER AE
        '\u{00C7}' => &['\u{00E7}'], // LATIN CAPITAL LETTER C WITH CEDILLA
        '\u{00C8}' => &['\u{00E8}'], // LATIN CAPITAL LETTER E WITH GRAVE
        '\u{00C9}' => &['\u{00E9}'], // LATIN CAPITAL LETTER E WITH ACUTE
        '\u{00CA}' => &['\u{00EA}'], // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
        '\u{00CB}' => &['\u{00EB}'], // LATIN CAPITAL LETTER E WITH DIAERESIS
        '\u{00CC}' => &['\u{00EC}'], // LATIN CAPITAL LETTER I WITH GRAVE
        '\u{00CD}' => &['\u{00ED}'], // LATIN CAPITAL LETTER I WITH ACUTE
        '\u{00CE}' => &['\u{00EE}'], // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
        '\u{00CF}' => &['\u{00EF}'], // LATIN CAPITAL LETTER I WITH DIAERESIS
        '\u{00D0}' => &['\u{00F0}'], // LATIN CAPITAL LETTER ETH
        '\u{00D1}' => &['\u{00F1}'], // LATIN CAPITAL LETTER N WITH TILDE
        '\u{00D2}' => &['\u{00F2}'], // LATIN CAPITAL LETTER O WITH GRAVE
        '\u{00D3}' => &['\u{00F3}'], // LATIN CAPITAL LETTER O WITH ACUTE
        '\u{00D4}' => &['\u{00F4}'], // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
        '\u{00D5}' => &['\u{00F5}'], // LATIN CAPITAL LETTER O WITH TILDE
        '\u{00D6}' => &['\u{00F6}'], // LATIN CAPITAL LETTER O WITH DIAERESIS
        '\u{00D8}' => &['\u{00F8}'], // LATIN CAPITAL LETTER O WITH STROKE
        '\u{00D9}' => &['\u{00F9}'], // LATIN CAPITAL LETTER U WITH GRAVE
        '\u{00DA}' => &['\u{00FA}'], // LATIN CAPITAL LETTER U WITH ACUTE
        '\u{00DB}' => &['\u{00FB}'], // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
        '\u{00DC}' => &['\u{00FC}'], // LATIN CAPITAL LETTER U WITH DIAERESIS
        '\u{00DD}' => &['\u{00FD}'], // LATIN CAPITAL LETTER Y WITH ACUTE
        '\u{00DE}' => &['\u{00FE}'], // LATIN CAPITAL LETTER THORN
        '\u{00DF}' => &['\u{0073}', '\u{0073}'], // LATIN SMALL LETTER SHARP S
        '\u{0100}' => &['\u{0101}'], // LATIN CAPITAL LETTER A WITH MACRON
        '\u{0102}' => &['\u{0103}'], // LATIN CAPITAL LETTER A WITH BREVE
        '\u{0104}' => &['\u{0105}'], // LATIN CAPITAL LETTER A WITH OGONEK
        '\u{0106}' => &['\u{0107}'], // LATIN CAPITAL LETTER C WITH ACUTE
        '\u{0108}' => &['\u{0109}'], // LATIN CAPITAL LETTER C WITH CIRCUMFLEX
        '\u{010A}' => &['\u{010B}'], // LATIN CAPITAL LETTER C WITH DOT ABOVE
        '\u{010C}' => &['\u{010D}'], // LATIN CAPITAL LETTER C WITH CARON
        '\u{010E}' => &['\u{010F}'], // LATIN CAPITAL LETTER D WITH CARON
        '\u{0110}' => &['\u{0111}'], // LATIN CAPITAL LETTER D WITH STROKE
        '\u{0112}' => &['\u{0113}'], // LATIN CAPITAL LETTER E WITH MACRON
        '\u{0114}' => &['\u{0115}'], // LATIN CAPITAL LETTER E WITH BREVE
        '\u{0116}' => &['\u{0117}'], // LATIN CAPITAL LETTER E WITH DOT ABOVE
        '\u{0118}' => &['\u{0119}'], // LATIN CAPITAL LETTER E WITH OGONEK
        '\u{011A}' => &['\u{011B}'], // LATIN CAPITAL LETTER E WITH CARON
        '\u{011C}' => &['\u{011D}'], // LATIN CAPITAL LETTER G WITH CIRCUMFLEX
        '\u{011E}' => &['\u{011F}'], // LATIN CAPITAL LETTER G WITH BREVE
        '\u{0120}' => &['\u{0121}'], // LATIN CAPITAL LETTER G WITH DOT ABOVE
        '\u{0122}' => &['\u{0123}'], // LATIN CAPITAL LETTER G WITH CEDILLA
        '\u{0124}' => &['\u{0125}'], // LATIN CAPITAL LETTER H WITH CIRCUMFLEX
        '\u{0126}' => &['\u{0127}'], // LATIN CAPITAL LETTER H WITH STROKE
        '\u{0128}' => &['\u{0129}'], // LATIN CAPITAL LETTER I WITH TILDE
        '\u{012A}' => &['\u{012B}'], // LATIN CAPITAL LETTER I WITH MACRON
        '\u{012C}' => &['\u{012D}'], // LATIN CAPITAL LETTER I WITH BREVE
        '\u{012E}' => &['\u{012F}'], // LATIN CAPITAL LETTER I WITH OGONEK
        // '\u{0130}' => &['\u{0069}', '\u{0307}'], // LATIN CAPITAL LETTER I WITH DOT ABOVE
        '\u{0130}' => &['\u{0069}'], // LATIN CAPITAL LETTER I WITH DOT ABOVE
        '\u{0132}' => &['\u{0133}'], // LATIN CAPITAL LIGATURE IJ
        '\u{0134}' => &['\u{0135}'], // LATIN CAPITAL LETTER J WITH CIRCUMFLEX
        '\u{0136}' => &['\u{0137}'], // LATIN CAPITAL LETTER K WITH CEDILLA
        '\u{0139}' => &['\u{013A}'], // LATIN CAPITAL LETTER L WITH ACUTE
        '\u{013B}' => &['\u{013C}'], // LATIN CAPITAL LETTER L WITH CEDILLA
        '\u{013D}' => &['\u{013E}'], // LATIN CAPITAL LETTER L WITH CARON
        '\u{013F}' => &['\u{0140}'], // LATIN CAPITAL LETTER L WITH MIDDLE DOT
        '\u{0141}' => &['\u{0142}'], // LATIN CAPITAL LETTER L WITH STROKE
        '\u{0143}' => &['\u{0144}'], // LATIN CAPITAL LETTER N WITH ACUTE
        '\u{0145}' => &['\u{0146}'], // LATIN CAPITAL LETTER N WITH CEDILLA
        '\u{0147}' => &['\u{0148}'], // LATIN CAPITAL LETTER N WITH CARON
        '\u{0149}' => &['\u{02BC}', '\u{006E}'], // LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
        '\u{014A}' => &['\u{014B}'], // LATIN CAPITAL LETTER ENG
        '\u{014C}' => &['\u{014D}'], // LATIN CAPITAL LETTER O WITH MACRON
        '\u{014E}' => &['\u{014F}'], // LATIN CAPITAL LETTER O WITH BREVE
        '\u{0150}' => &['\u{0151}'], // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
        '\u{0152}' => &['\u{0153}'], // LATIN CAPITAL LIGATURE OE
        '\u{0154}' => &['\u{0155}'], // LATIN CAPITAL LETTER R WITH ACUTE
        '\u{0156}' => &['\u{0157}'], // LATIN CAPITAL LETTER R WITH CEDILLA
        '\u{0158}' => &['\u{0159}'], // LATIN CAPITAL LETTER R WITH CARON
        '\u{015A}' => &['\u{015B}'], // LATIN CAPITAL LETTER S WITH ACUTE
        '\u{015C}' => &['\u{015D}'], // LATIN CAPITAL LETTER S WITH CIRCUMFLEX
        '\u{015E}' => &['\u{015F}'], // LATIN CAPITAL LETTER S WITH CEDILLA
        '\u{0160}' => &['\u{0161}'], // LATIN CAPITAL LETTER S WITH CARON
        '\u{0162}' => &['\u{0163}'], // LATIN CAPITAL LETTER T WITH CEDILLA
        '\u{0164}' => &['\u{0165}'], // LATIN CAPITAL LETTER T WITH CARON
        '\u{0166}' => &['\u{0167}'], // LATIN CAPITAL LETTER T WITH STROKE
        '\u{0168}' => &['\u{0169}'], // LATIN CAPITAL LETTER U WITH TILDE
        '\u{016A}' => &['\u{016B}'], // LATIN CAPITAL LETTER U WITH MACRON
        '\u{016C}' => &['\u{016D}'], // LATIN CAPITAL LETTER U WITH BREVE
        '\u{016E}' => &['\u{016F}'], // LATIN CAPITAL LETTER U WITH RING ABOVE
        '\u{0170}' => &['\u{0171}'], // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
        '\u{0172}' => &['\u{0173}'], // LATIN CAPITAL LETTER U WITH OGONEK
        '\u{0174}' => &['\u{0175}'], // LATIN CAPITAL LETTER W WITH CIRCUMFLEX
        '\u{0176}' => &['\u{0177}'], // LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
        '\u{0178}' => &['\u{00FF}'], // LATIN CAPITAL LETTER Y WITH DIAERESIS
        '\u{0179}' => &['\u{017A}'], // LATIN CAPITAL LETTER Z WITH ACUTE
        '\u{017B}' => &['\u{017C}'], // LATIN CAPITAL LETTER Z WITH DOT ABOVE
        '\u{017D}' => &['\u{017E}'], // LATIN CAPITAL LETTER Z WITH CARON
        '\u{017F}' => &['\u{0073}'], // LATIN SMALL LETTER LONG S
        '\u{0181}' => &['\u{0253}'], // LATIN CAPITAL LETTER B WITH HOOK
        '\u{0182}' => &['\u{0183}'], // LATIN CAPITAL LETTER B WITH TOPBAR
        '\u{0184}' => &['\u{0185}'], // LATIN CAPITAL LETTER TONE SIX
        '\u{0186}' => &['\u{0254}'], // LATIN CAPITAL LETTER OPEN O
        '\u{0187}' => &['\u{0188}'], // LATIN CAPITAL LETTER C WITH HOOK
        '\u{0189}' => &['\u{0256}'], // LATIN CAPITAL LETTER AFRICAN D
        '\u{018A}' => &['\u{0257}'], // LATIN CAPITAL LETTER D WITH HOOK
        '\u{018B}' => &['\u{018C}'], // LATIN CAPITAL LETTER D WITH TOPBAR
        '\u{018E}' => &['\u{01DD}'], // LATIN CAPITAL LETTER REVERSED E
        '\u{018F}' => &['\u{0259}'], // LATIN CAPITAL LETTER SCHWA
        '\u{0190}' => &['\u{025B}'], // LATIN CAPITAL LETTER OPEN E
        '\u{0191}' => &['\u{0192}'], // LATIN CAPITAL LETTER F WITH HOOK
        '\u{0193}' => &['\u{0260}'], // LATIN CAPITAL LETTER G WITH HOOK
        '\u{0194}' => &['\u{0263}'], // LATIN CAPITAL LETTER GAMMA
        '\u{0196}' => &['\u{0269}'], // LATIN CAPITAL LETTER IOTA
        '\u{0197}' => &['\u{0268}'], // LATIN CAPITAL LETTER I WITH STROKE
        '\u{0198}' => &['\u{0199}'], // LATIN CAPITAL LETTER K WITH HOOK
        '\u{019C}' => &['\u{026F}'], // LATIN CAPITAL LETTER TURNED M
        '\u{019D}' => &['\u{0272}'], // LATIN CAPITAL LETTER N WITH LEFT HOOK
        '\u{019F}' => &['\u{0275}'], // LATIN CAPITAL LETTER O WITH MIDDLE TILDE
        '\u{01A0}' => &['\u{01A1}'], // LATIN CAPITAL LETTER O WITH HORN
        '\u{01A2}' => &['\u{01A3}'], // LATIN CAPITAL LETTER OI
        '\u{01A4}' => &['\u{01A5}'], // LATIN CAPITAL LETTER P WITH HOOK
        '\u{01A6}' => &['\u{0280}'], // LATIN LETTER YR
        '\u{01A7}' => &['\u{01A8}'], // LATIN CAPITAL LETTER TONE TWO
        '\u{01A9}' => &['\u{0283}'], // LATIN CAPITAL LETTER ESH
        '\u{01AC}' => &['\u{01AD}'], // LATIN CAPITAL LETTER T WITH HOOK
        '\u{01AE}' => &['\u{0288}'], // LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
        '\u{01AF}' => &['\u{01B0}'], // LATIN CAPITAL LETTER U WITH HORN
        '\u{01B1}' => &['\u{028A}'], // LATIN CAPITAL LETTER UPSILON
        '\u{01B2}' => &['\u{028B}'], // LATIN CAPITAL LETTER V WITH HOOK
        '\u{01B3}' => &['\u{01B4}'], // LATIN CAPITAL LETTER Y WITH HOOK
        '\u{01B5}' => &['\u{01B6}'], // LATIN CAPITAL LETTER Z WITH STROKE
        '\u{01B7}' => &['\u{0292}'], // LATIN CAPITAL LETTER EZH
        '\u{01B8}' => &['\u{01B9}'], // LATIN CAPITAL LETTER EZH REVERSED
        '\u{01BC}' => &['\u{01BD}'], // LATIN CAPITAL LETTER TONE FIVE
        '\u{01C4}' => &['\u{01C6}'], // LATIN CAPITAL LETTER DZ WITH CARON
        '\u{01C5}' => &['\u{01C6}'], // LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
        '\u{01C7}' => &['\u{01C9}'], // LATIN CAPITAL LETTER LJ
        '\u{01C8}' => &['\u{01C9}'], // LATIN CAPITAL LETTER L WITH SMALL LETTER J
        '\u{01CA}' => &['\u{01CC}'], // LATIN CAPITAL LETTER NJ
        '\u{01CB}' => &['\u{01CC}'], // LATIN CAPITAL LETTER N WITH SMALL LETTER J
        '\u{01CD}' => &['\u{01CE}'], // LATIN CAPITAL LETTER A WITH CARON
        '\u{01CF}' => &['\u{01D0}'], // LATIN CAPITAL LETTER I WITH CARON
        '\u{01D1}' => &['\u{01D2}'], // LATIN CAPITAL LETTER O WITH CARON
        '\u{01D3}' => &['\u{01D4}'], // LATIN CAPITAL LETTER U WITH CARON
        '\u{01D5}' => &['\u{01D6}'], // LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
        '\u{01D7}' => &['\u{01D8}'], // LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
        '\u{01D9}' => &['\u{01DA}'], // LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
        '\u{01DB}' => &['\u{01DC}'], // LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
        '\u{01DE}' => &['\u{01DF}'], // LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
        '\u{01E0}' => &['\u{01E1}'], // LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
        '\u{01E2}' => &['\u{01E3}'], // LATIN CAPITAL LETTER AE WITH MACRON
        '\u{01E4}' => &['\u{01E5}'], // LATIN CAPITAL LETTER G WITH STROKE
        '\u{01E6}' => &['\u{01E7}'], // LATIN CAPITAL LETTER G WITH CARON
        '\u{01E8}' => &['\u{01E9}'], // LATIN CAPITAL LETTER K WITH CARON
        '\u{01EA}' => &['\u{01EB}'], // LATIN CAPITAL LETTER O WITH OGONEK
        '\u{01EC}' => &['\u{01ED}'], // LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
        '\u{01EE}' => &['\u{01EF}'], // LATIN CAPITAL LETTER EZH WITH CARON
        '\u{01F0}' => &['\u{006A}', '\u{030C}'], // LATIN SMALL LETTER J WITH CARON
        '\u{01F1}' => &['\u{01F3}'], // LATIN CAPITAL LETTER DZ
        '\u{01F2}' => &['\u{01F3}'], // LATIN CAPITAL LETTER D WITH SMALL LETTER Z
        '\u{01F4}' => &['\u{01F5}'], // LATIN CAPITAL LETTER G WITH ACUTE
        '\u{01F6}' => &['\u{0195}'], // LATIN CAPITAL LETTER HWAIR
        '\u{01F7}' => &['\u{01BF}'], // LATIN CAPITAL LETTER WYNN
        '\u{01F8}' => &['\u{01F9}'], // LATIN CAPITAL LETTER N WITH GRAVE
        '\u{01FA}' => &['\u{01FB}'], // LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
        '\u{01FC}' => &['\u{01FD}'], // LATIN CAPITAL LETTER AE WITH ACUTE
        '\u{01FE}' => &['\u{01FF}'], // LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
        '\u{0200}' => &['\u{0201}'], // LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
        '\u{0202}' => &['\u{0203}'], // LATIN CAPITAL LETTER A WITH INVERTED BREVE
        '\u{0204}' => &['\u{0205}'], // LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
        '\u{0206}' => &['\u{0207}'], // LATIN CAPITAL LETTER E WITH INVERTED BREVE
        '\u{0208}' => &['\u{0209}'], // LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
        '\u{020A}' => &['\u{020B}'], // LATIN CAPITAL LETTER I WITH INVERTED BREVE
        '\u{020C}' => &['\u{020D}'], // LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
        '\u{020E}' => &['\u{020F}'], // LATIN CAPITAL LETTER O WITH INVERTED BREVE
        '\u{0210}' => &['\u{0211}'], // LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
        '\u{0212}' => &['\u{0213}'], // LATIN CAPITAL LETTER R WITH INVERTED BREVE
        '\u{0214}' => &['\u{0215}'], // LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
        '\u{0216}' => &['\u{0217}'], // LATIN CAPITAL LETTER U WITH INVERTED BREVE
        '\u{0218}' => &['\u{0219}'], // LATIN CAPITAL LETTER S WITH COMMA BELOW
        '\u{021A}' => &['\u{021B}'], // LATIN CAPITAL LETTER T WITH COMMA BELOW
        '\u{021C}' => &['\u{021D}'], // LATIN CAPITAL LETTER YOGH
        '\u{021E}' => &['\u{021F}'], // LATIN CAPITAL LETTER H WITH CARON
        '\u{0220}' => &['\u{019E}'], // LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
        '\u{0222}' => &['\u{0223}'], // LATIN CAPITAL LETTER OU
        '\u{0224}' => &['\u{0225}'], // LATIN CAPITAL LETTER Z WITH HOOK
        '\u{0226}' => &['\u{0227}'], // LATIN CAPITAL LETTER A WITH DOT ABOVE
        '\u{0228}' => &['\u{0229}'], // LATIN CAPITAL LETTER E WITH CEDILLA
        '\u{022A}' => &['\u{022B}'], // LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
        '\u{022C}' => &['\u{022D}'], // LATIN CAPITAL LETTER O WITH TILDE AND MACRON
        '\u{022E}' => &['\u{022F}'], // LATIN CAPITAL LETTER O WITH DOT ABOVE
        '\u{0230}' => &['\u{0231}'], // LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
        '\u{0232}' => &['\u{0233}'], // LATIN CAPITAL LETTER Y WITH MACRON
        '\u{023A}' => &['\u{2C65}'], // LATIN CAPITAL LETTER A WITH STROKE
        '\u{023B}' => &['\u{023C}'], // LATIN CAPITAL LETTER C WITH STROKE
        '\u{023D}' => &['\u{019A}'], // LATIN CAPITAL LETTER L WITH BAR
        '\u{023E}' => &['\u{2C66}'], // LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
        '\u{0241}' => &['\u{0242}'], // LATIN CAPITAL LETTER GLOTTAL STOP
        '\u{0243}' => &['\u{0180}'], // LATIN CAPITAL LETTER B WITH STROKE
        '\u{0244}' => &['\u{0289}'], // LATIN CAPITAL LETTER U BAR
        '\u{0245}' => &['\u{028C}'], // LATIN CAPITAL LETTER TURNED V
        '\u{0246}' => &['\u{0247}'], // LATIN CAPITAL LETTER E WITH STROKE
        '\u{0248}' => &['\u{0249}'], // LATIN CAPITAL LETTER J WITH STROKE
        '\u{024A}' => &['\u{024B}'], // LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
        '\u{024C}' => &['\u{024D}'], // LATIN CAPITAL LETTER R WITH STROKE
        '\u{024E}' => &['\u{024F}'], // LATIN CAPITAL LETTER Y WITH STROKE
        '\u{0345}' => &['\u{03B9}'], // COMBINING GREEK YPOGEGRAMMENI
        '\u{0370}' => &['\u{0371}'], // GREEK CAPITAL LETTER HETA
        '\u{0372}' => &['\u{0373}'], // GREEK CAPITAL LETTER ARCHAIC SAMPI
        '\u{0376}' => &['\u{0377}'], // GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
        '\u{037F}' => &['\u{03F3}'], // GREEK CAPITAL LETTER YOT
        '\u{0386}' => &['\u{03AC}'], // GREEK CAPITAL LETTER ALPHA WITH TONOS
        '\u{0388}' => &['\u{03AD}'], // GREEK CAPITAL LETTER EPSILON WITH TONOS
        '\u{0389}' => &['\u{03AE}'], // GREEK CAPITAL LETTER ETA WITH TONOS
        '\u{038A}' => &['\u{03AF}'], // GREEK CAPITAL LETTER IOTA WITH TONOS
        '\u{038C}' => &['\u{03CC}'], // GREEK CAPITAL LETTER OMICRON WITH TONOS
        '\u{038E}' => &['\u{03CD}'], // GREEK CAPITAL LETTER UPSILON WITH TONOS
        '\u{038F}' => &['\u{03CE}'], // GREEK CAPITAL LETTER OMEGA WITH TONOS
        '\u{0390}' => &['\u{03B9}', '\u{0308}', '\u{0301}'], // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
        '\u{0391}' => &['\u{03B1}'],                         // GREEK CAPITAL LETTER ALPHA
        '\u{0392}' => &['\u{03B2}'],                         // GREEK CAPITAL LETTER BETA
        '\u{0393}' => &['\u{03B3}'],                         // GREEK CAPITAL LETTER GAMMA
        '\u{0394}' => &['\u{03B4}'],                         // GREEK CAPITAL LETTER DELTA
        '\u{0395}' => &['\u{03B5}'],                         // GREEK CAPITAL LETTER EPSILON
        '\u{0396}' => &['\u{03B6}'],                         // GREEK CAPITAL LETTER ZETA
        '\u{0397}' => &['\u{03B7}'],                         // GREEK CAPITAL LETTER ETA
        '\u{0398}' => &['\u{03B8}'],                         // GREEK CAPITAL LETTER THETA
        '\u{0399}' => &['\u{03B9}'],                         // GREEK CAPITAL LETTER IOTA
        '\u{039A}' => &['\u{03BA}'],                         // GREEK CAPITAL LETTER KAPPA
        '\u{039B}' => &['\u{03BB}'],                         // GREEK CAPITAL LETTER LAMDA
        '\u{039C}' => &['\u{03BC}'],                         // GREEK CAPITAL LETTER MU
        '\u{039D}' => &['\u{03BD}'],                         // GREEK CAPITAL LETTER NU
        '\u{039E}' => &['\u{03BE}'],                         // GREEK CAPITAL LETTER XI
        '\u{039F}' => &['\u{03BF}'],                         // GREEK CAPITAL LETTER OMICRON
        '\u{03A0}' => &['\u{03C0}'],                         // GREEK CAPITAL LETTER PI
        '\u{03A1}' => &['\u{03C1}'],                         // GREEK CAPITAL LETTER RHO
        '\u{03A3}' => &['\u{03C3}'],                         // GREEK CAPITAL LETTER SIGMA
        '\u{03A4}' => &['\u{03C4}'],                         // GREEK CAPITAL LETTER TAU
        '\u{03A5}' => &['\u{03C5}'],                         // GREEK CAPITAL LETTER UPSILON
        '\u{03A6}' => &['\u{03C6}'],                         // GREEK CAPITAL LETTER PHI
        '\u{03A7}' => &['\u{03C7}'],                         // GREEK CAPITAL LETTER CHI
        '\u{03A8}' => &['\u{03C8}'],                         // GREEK CAPITAL LETTER PSI
        '\u{03A9}' => &['\u{03C9}'],                         // GREEK CAPITAL LETTER OMEGA
        '\u{03AA}' => &['\u{03CA}'], // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
        '\u{03AB}' => &['\u{03CB}'], // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
        '\u{03B0}' => &['\u{03C5}', '\u{0308}', '\u{0301}'], // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
        '\u{03C2}' => &['\u{03C3}'],                         // GREEK SMALL LETTER FINAL SIGMA
        '\u{03CF}' => &['\u{03D7}'],                         // GREEK CAPITAL KAI SYMBOL
        '\u{03D0}' => &['\u{03B2}'],                         // GREEK BETA SYMBOL
        '\u{03D1}' => &['\u{03B8}'],                         // GREEK THETA SYMBOL
        '\u{03D5}' => &['\u{03C6}'],                         // GREEK PHI SYMBOL
        '\u{03D6}' => &['\u{03C0}'],                         // GREEK PI SYMBOL
        '\u{03D8}' => &['\u{03D9}'],                         // GREEK LETTER ARCHAIC KOPPA
        '\u{03DA}' => &['\u{03DB}'],                         // GREEK LETTER STIGMA
        '\u{03DC}' => &['\u{03DD}'],                         // GREEK LETTER DIGAMMA
        '\u{03DE}' => &['\u{03DF}'],                         // GREEK LETTER KOPPA
        '\u{03E0}' => &['\u{03E1}'],                         // GREEK LETTER SAMPI
        '\u{03E2}' => &['\u{03E3}'],                         // COPTIC CAPITAL LETTER SHEI
        '\u{03E4}' => &['\u{03E5}'],                         // COPTIC CAPITAL LETTER FEI
        '\u{03E6}' => &['\u{03E7}'],                         // COPTIC CAPITAL LETTER KHEI
        '\u{03E8}' => &['\u{03E9}'],                         // COPTIC CAPITAL LETTER HORI
        '\u{03EA}' => &['\u{03EB}'],                         // COPTIC CAPITAL LETTER GANGIA
        '\u{03EC}' => &['\u{03ED}'],                         // COPTIC CAPITAL LETTER SHIMA
        '\u{03EE}' => &['\u{03EF}'],                         // COPTIC CAPITAL LETTER DEI
        '\u{03F0}' => &['\u{03BA}'],                         // GREEK KAPPA SYMBOL
        '\u{03F1}' => &['\u{03C1}'],                         // GREEK RHO SYMBOL
        '\u{03F4}' => &['\u{03B8}'],                         // GREEK CAPITAL THETA SYMBOL
        '\u{03F5}' => &['\u{03B5}'],                         // GREEK LUNATE EPSILON SYMBOL
        '\u{03F7}' => &['\u{03F8}'],                         // GREEK CAPITAL LETTER SHO
        '\u{03F9}' => &['\u{03F2}'],                         // GREEK CAPITAL LUNATE SIGMA SYMBOL
        '\u{03FA}' => &['\u{03FB}'],                         // GREEK CAPITAL LETTER SAN
        '\u{03FD}' => &['\u{037B}'], // GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL
        '\u{03FE}' => &['\u{037C}'], // GREEK CAPITAL DOTTED LUNATE SIGMA SYMBOL
        '\u{03FF}' => &['\u{037D}'], // GREEK CAPITAL REVERSED DOTTED LUNATE SIGMA SYMBOL
        '\u{0400}' => &['\u{0450}'], // CYRILLIC CAPITAL LETTER IE WITH GRAVE
        '\u{0401}' => &['\u{0451}'], // CYRILLIC CAPITAL LETTER IO
        '\u{0402}' => &['\u{0452}'], // CYRILLIC CAPITAL LETTER DJE
        '\u{0403}' => &['\u{0453}'], // CYRILLIC CAPITAL LETTER GJE
        '\u{0404}' => &['\u{0454}'], // CYRILLIC CAPITAL LETTER UKRAINIAN IE
        '\u{0405}' => &['\u{0455}'], // CYRILLIC CAPITAL LETTER DZE
        '\u{0406}' => &['\u{0456}'], // CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
        '\u{0407}' => &['\u{0457}'], // CYRILLIC CAPITAL LETTER YI
        '\u{0408}' => &['\u{0458}'], // CYRILLIC CAPITAL LETTER JE
        '\u{0409}' => &['\u{0459}'], // CYRILLIC CAPITAL LETTER LJE
        '\u{040A}' => &['\u{045A}'], // CYRILLIC CAPITAL LETTER NJE
        '\u{040B}' => &['\u{045B}'], // CYRILLIC CAPITAL LETTER TSHE
        '\u{040C}' => &['\u{045C}'], // CYRILLIC CAPITAL LETTER KJE
        '\u{040D}' => &['\u{045D}'], // CYRILLIC CAPITAL LETTER I WITH GRAVE
        '\u{040E}' => &['\u{045E}'], // CYRILLIC CAPITAL LETTER SHORT U
        '\u{040F}' => &['\u{045F}'], // CYRILLIC CAPITAL LETTER DZHE
        '\u{0410}' => &['\u{0430}'], // CYRILLIC CAPITAL LETTER A
        '\u{0411}' => &['\u{0431}'], // CYRILLIC CAPITAL LETTER BE
        '\u{0412}' => &['\u{0432}'], // CYRILLIC CAPITAL LETTER VE
        '\u{0413}' => &['\u{0433}'], // CYRILLIC CAPITAL LETTER GHE
        '\u{0414}' => &['\u{0434}'], // CYRILLIC CAPITAL LETTER DE
        '\u{0415}' => &['\u{0435}'], // CYRILLIC CAPITAL LETTER IE
        '\u{0416}' => &['\u{0436}'], // CYRILLIC CAPITAL LETTER ZHE
        '\u{0417}' => &['\u{0437}'], // CYRILLIC CAPITAL LETTER ZE
        '\u{0418}' => &['\u{0438}'], // CYRILLIC CAPITAL LETTER I
        '\u{0419}' => &['\u{0439}'], // CYRILLIC CAPITAL LETTER SHORT I
        '\u{041A}' => &['\u{043A}'], // CYRILLIC CAPITAL LETTER KA
        '\u{041B}' => &['\u{043B}'], // CYRILLIC CAPITAL LETTER EL
        '\u{041C}' => &['\u{043C}'], // CYRILLIC CAPITAL LETTER EM
        '\u{041D}' => &['\u{043D}'], // CYRILLIC CAPITAL LETTER EN
        '\u{041E}' => &['\u{043E}'], // CYRILLIC CAPITAL LETTER O
        '\u{041F}' => &['\u{043F}'], // CYRILLIC CAPITAL LETTER PE
        '\u{0420}' => &['\u{0440}'], // CYRILLIC CAPITAL LETTER ER
        '\u{0421}' => &['\u{0441}'], // CYRILLIC CAPITAL LETTER ES
        '\u{0422}' => &['\u{0442}'], // CYRILLIC CAPITAL LETTER TE
        '\u{0423}' => &['\u{0443}'], // CYRILLIC CAPITAL LETTER U
        '\u{0424}' => &['\u{0444}'], // CYRILLIC CAPITAL LETTER EF
        '\u{0425}' => &['\u{0445}'], // CYRILLIC CAPITAL LETTER HA
        '\u{0426}' => &['\u{0446}'], // CYRILLIC CAPITAL LETTER TSE
        '\u{0427}' => &['\u{0447}'], // CYRILLIC CAPITAL LETTER CHE
        '\u{0428}' => &['\u{0448}'], // CYRILLIC CAPITAL LETTER SHA
        '\u{0429}' => &['\u{0449}'], // CYRILLIC CAPITAL LETTER SHCHA
        '\u{042A}' => &['\u{044A}'], // CYRILLIC CAPITAL LETTER HARD SIGN
        '\u{042B}' => &['\u{044B}'], // CYRILLIC CAPITAL LETTER YERU
        '\u{042C}' => &['\u{044C}'], // CYRILLIC CAPITAL LETTER SOFT SIGN
        '\u{042D}' => &['\u{044D}'], // CYRILLIC CAPITAL LETTER E
        '\u{042E}' => &['\u{044E}'], // CYRILLIC CAPITAL LETTER YU
        '\u{042F}' => &['\u{044F}'], // CYRILLIC CAPITAL LETTER YA
        '\u{0460}' => &['\u{0461}'], // CYRILLIC CAPITAL LETTER OMEGA
        '\u{0462}' => &['\u{0463}'], // CYRILLIC CAPITAL LETTER YAT
        '\u{0464}' => &['\u{0465}'], // CYRILLIC CAPITAL LETTER IOTIFIED E
        '\u{0466}' => &['\u{0467}'], // CYRILLIC CAPITAL LETTER LITTLE YUS
        '\u{0468}' => &['\u{0469}'], // CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
        '\u{046A}' => &['\u{046B}'], // CYRILLIC CAPITAL LETTER BIG YUS
        '\u{046C}' => &['\u{046D}'], // CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
        '\u{046E}' => &['\u{046F}'], // CYRILLIC CAPITAL LETTER KSI
        '\u{0470}' => &['\u{0471}'], // CYRILLIC CAPITAL LETTER PSI
        '\u{0472}' => &['\u{0473}'], // CYRILLIC CAPITAL LETTER FITA
        '\u{0474}' => &['\u{0475}'], // CYRILLIC CAPITAL LETTER IZHITSA
        '\u{0476}' => &['\u{0477}'], // CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
        '\u{0478}' => &['\u{0479}'], // CYRILLIC CAPITAL LETTER UK
        '\u{047A}' => &['\u{047B}'], // CYRILLIC CAPITAL LETTER ROUND OMEGA
        '\u{047C}' => &['\u{047D}'], // CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
        '\u{047E}' => &['\u{047F}'], // CYRILLIC CAPITAL LETTER OT
        '\u{0480}' => &['\u{0481}'], // CYRILLIC CAPITAL LETTER KOPPA
        '\u{048A}' => &['\u{048B}'], // CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
        '\u{048C}' => &['\u{048D}'], // CYRILLIC CAPITAL LETTER SEMISOFT SIGN
        '\u{048E}' => &['\u{048F}'], // CYRILLIC CAPITAL LETTER ER WITH TICK
        '\u{0490}' => &['\u{0491}'], // CYRILLIC CAPITAL LETTER GHE WITH UPTURN
        '\u{0492}' => &['\u{0493}'], // CYRILLIC CAPITAL LETTER GHE WITH STROKE
        '\u{0494}' => &['\u{0495}'], // CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
        '\u{0496}' => &['\u{0497}'], // CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
        '\u{0498}' => &['\u{0499}'], // CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
        '\u{049A}' => &['\u{049B}'], // CYRILLIC CAPITAL LETTER KA WITH DESCENDER
        '\u{049C}' => &['\u{049D}'], // CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
        '\u{049E}' => &['\u{049F}'], // CYRILLIC CAPITAL LETTER KA WITH STROKE
        '\u{04A0}' => &['\u{04A1}'], // CYRILLIC CAPITAL LETTER BASHKIR KA
        '\u{04A2}' => &['\u{04A3}'], // CYRILLIC CAPITAL LETTER EN WITH DESCENDER
        '\u{04A4}' => &['\u{04A5}'], // CYRILLIC CAPITAL LIGATURE EN GHE
        '\u{04A6}' => &['\u{04A7}'], // CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
        '\u{04A8}' => &['\u{04A9}'], // CYRILLIC CAPITAL LETTER ABKHASIAN HA
        '\u{04AA}' => &['\u{04AB}'], // CYRILLIC CAPITAL LETTER ES WITH DESCENDER
        '\u{04AC}' => &['\u{04AD}'], // CYRILLIC CAPITAL LETTER TE WITH DESCENDER
        '\u{04AE}' => &['\u{04AF}'], // CYRILLIC CAPITAL LETTER STRAIGHT U
        '\u{04B0}' => &['\u{04B1}'], // CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
        '\u{04B2}' => &['\u{04B3}'], // CYRILLIC CAPITAL LETTER HA WITH DESCENDER
        '\u{04B4}' => &['\u{04B5}'], // CYRILLIC CAPITAL LIGATURE TE TSE
        '\u{04B6}' => &['\u{04B7}'], // CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
        '\u{04B8}' => &['\u{04B9}'], // CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
        '\u{04BA}' => &['\u{04BB}'], // CYRILLIC CAPITAL LETTER SHHA
        '\u{04BC}' => &['\u{04BD}'], // CYRILLIC CAPITAL LETTER ABKHASIAN CHE
        '\u{04BE}' => &['\u{04BF}'], // CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
        '\u{04C0}' => &['\u{04CF}'], // CYRILLIC LETTER PALOCHKA
        '\u{04C1}' => &['\u{04C2}'], // CYRILLIC CAPITAL LETTER ZHE WITH BREVE
        '\u{04C3}' => &['\u{04C4}'], // CYRILLIC CAPITAL LETTER KA WITH HOOK
        '\u{04C5}' => &['\u{04C6}'], // CYRILLIC CAPITAL LETTER EL WITH TAIL
        '\u{04C7}' => &['\u{04C8}'], // CYRILLIC CAPITAL LETTER EN WITH HOOK
        '\u{04C9}' => &['\u{04CA}'], // CYRILLIC CAPITAL LETTER EN WITH TAIL
        '\u{04CB}' => &['\u{04CC}'], // CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
        '\u{04CD}' => &['\u{04CE}'], // CYRILLIC CAPITAL LETTER EM WITH TAIL
        '\u{04D0}' => &['\u{04D1}'], // CYRILLIC CAPITAL LETTER A WITH BREVE
        '\u{04D2}' => &['\u{04D3}'], // CYRILLIC CAPITAL LETTER A WITH DIAERESIS
        '\u{04D4}' => &['\u{04D5}'], // CYRILLIC CAPITAL LIGATURE A IE
        '\u{04D6}' => &['\u{04D7}'], // CYRILLIC CAPITAL LETTER IE WITH BREVE
        '\u{04D8}' => &['\u{04D9}'], // CYRILLIC CAPITAL LETTER SCHWA
        '\u{04DA}' => &['\u{04DB}'], // CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
        '\u{04DC}' => &['\u{04DD}'], // CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
        '\u{04DE}' => &['\u{04DF}'], // CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
        '\u{04E0}' => &['\u{04E1}'], // CYRILLIC CAPITAL LETTER ABKHASIAN DZE
        '\u{04E2}' => &['\u{04E3}'], // CYRILLIC CAPITAL LETTER I WITH MACRON
        '\u{04E4}' => &['\u{04E5}'], // CYRILLIC CAPITAL LETTER I WITH DIAERESIS
        '\u{04E6}' => &['\u{04E7}'], // CYRILLIC CAPITAL LETTER O WITH DIAERESIS
        '\u{04E8}' => &['\u{04E9}'], // CYRILLIC CAPITAL LETTER BARRED O
        '\u{04EA}' => &['\u{04EB}'], // CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
        '\u{04EC}' => &['\u{04ED}'], // CYRILLIC CAPITAL LETTER E WITH DIAERESIS
        '\u{04EE}' => &['\u{04EF}'], // CYRILLIC CAPITAL LETTER U WITH MACRON
        '\u{04F0}' => &['\u{04F1}'], // CYRILLIC CAPITAL LETTER U WITH DIAERESIS
        '\u{04F2}' => &['\u{04F3}'], // CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
        '\u{04F4}' => &['\u{04F5}'], // CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
        '\u{04F6}' => &['\u{04F7}'], // CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
        '\u{04F8}' => &['\u{04F9}'], // CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
        '\u{04FA}' => &['\u{04FB}'], // CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
        '\u{04FC}' => &['\u{04FD}'], // CYRILLIC CAPITAL LETTER HA WITH HOOK
        '\u{04FE}' => &['\u{04FF}'], // CYRILLIC CAPITAL LETTER HA WITH STROKE
        '\u{0500}' => &['\u{0501}'], // CYRILLIC CAPITAL LETTER KOMI DE
        '\u{0502}' => &['\u{0503}'], // CYRILLIC CAPITAL LETTER KOMI DJE
        '\u{0504}' => &['\u{0505}'], // CYRILLIC CAPITAL LETTER KOMI ZJE
        '\u{0506}' => &['\u{0507}'], // CYRILLIC CAPITAL LETTER KOMI DZJE
        '\u{0508}' => &['\u{0509}'], // CYRILLIC CAPITAL LETTER KOMI LJE
        '\u{050A}' => &['\u{050B}'], // CYRILLIC CAPITAL LETTER KOMI NJE
        '\u{050C}' => &['\u{050D}'], // CYRILLIC CAPITAL LETTER KOMI SJE
        '\u{050E}' => &['\u{050F}'], // CYRILLIC CAPITAL LETTER KOMI TJE
        '\u{0510}' => &['\u{0511}'], // CYRILLIC CAPITAL LETTER REVERSED ZE
        '\u{0512}' => &['\u{0513}'], // CYRILLIC CAPITAL LETTER EL WITH HOOK
        '\u{0514}' => &['\u{0515}'], // CYRILLIC CAPITAL LETTER LHA
        '\u{0516}' => &['\u{0517}'], // CYRILLIC CAPITAL LETTER RHA
        '\u{0518}' => &['\u{0519}'], // CYRILLIC CAPITAL LETTER YAE
        '\u{051A}' => &['\u{051B}'], // CYRILLIC CAPITAL LETTER QA
        '\u{051C}' => &['\u{051D}'], // CYRILLIC CAPITAL LETTER WE
        '\u{051E}' => &['\u{051F}'], // CYRILLIC CAPITAL LETTER ALEUT KA
        '\u{0520}' => &['\u{0521}'], // CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
        '\u{0522}' => &['\u{0523}'], // CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
        '\u{0524}' => &['\u{0525}'], // CYRILLIC CAPITAL LETTER PE WITH DESCENDER
        '\u{0526}' => &['\u{0527}'], // CYRILLIC CAPITAL LETTER SHHA WITH DESCENDER
        '\u{0528}' => &['\u{0529}'], // CYRILLIC CAPITAL LETTER EN WITH LEFT HOOK
        '\u{052A}' => &['\u{052B}'], // CYRILLIC CAPITAL LETTER DZZHE
        '\u{052C}' => &['\u{052D}'], // CYRILLIC CAPITAL LETTER DCHE
        '\u{052E}' => &['\u{052F}'], // CYRILLIC CAPITAL LETTER EL WITH DESCENDER
        '\u{0531}' => &['\u{0561}'], // ARMENIAN CAPITAL LETTER AYB
        '\u{0532}' => &['\u{0562}'], // ARMENIAN CAPITAL LETTER BEN
        '\u{0533}' => &['\u{0563}'], // ARMENIAN CAPITAL LETTER GIM
        '\u{0534}' => &['\u{0564}'], // ARMENIAN CAPITAL LETTER DA
        '\u{0535}' => &['\u{0565}'], // ARMENIAN CAPITAL LETTER ECH
        '\u{0536}' => &['\u{0566}'], // ARMENIAN CAPITAL LETTER ZA
        '\u{0537}' => &['\u{0567}'], // ARMENIAN CAPITAL LETTER EH
        '\u{0538}' => &['\u{0568}'], // ARMENIAN CAPITAL LETTER ET
        '\u{0539}' => &['\u{0569}'], // ARMENIAN CAPITAL LETTER TO
        '\u{053A}' => &['\u{056A}'], // ARMENIAN CAPITAL LETTER ZHE
        '\u{053B}' => &['\u{056B}'], // ARMENIAN CAPITAL LETTER INI
        '\u{053C}' => &['\u{056C}'], // ARMENIAN CAPITAL LETTER LIWN
        '\u{053D}' => &['\u{056D}'], // ARMENIAN CAPITAL LETTER XEH
        '\u{053E}' => &['\u{056E}'], // ARMENIAN CAPITAL LETTER CA
        '\u{053F}' => &['\u{056F}'], // ARMENIAN CAPITAL LETTER KEN
        '\u{0540}' => &['\u{0570}'], // ARMENIAN CAPITAL LETTER HO
        '\u{0541}' => &['\u{0571}'], // ARMENIAN CAPITAL LETTER JA
        '\u{0542}' => &['\u{0572}'], // ARMENIAN CAPITAL LETTER GHAD
        '\u{0543}' => &['\u{0573}'], // ARMENIAN CAPITAL LETTER CHEH
        '\u{0544}' => &['\u{0574}'], // ARMENIAN CAPITAL LETTER MEN
        '\u{0545}' => &['\u{0575}'], // ARMENIAN CAPITAL LETTER YI
        '\u{0546}' => &['\u{0576}'], // ARMENIAN CAPITAL LETTER NOW
        '\u{0547}' => &['\u{0577}'], // ARMENIAN CAPITAL LETTER SHA
        '\u{0548}' => &['\u{0578}'], // ARMENIAN CAPITAL LETTER VO
        '\u{0549}' => &['\u{0579}'], // ARMENIAN CAPITAL LETTER CHA
        '\u{054A}' => &['\u{057A}'], // ARMENIAN CAPITAL LETTER PEH
        '\u{054B}' => &['\u{057B}'], // ARMENIAN CAPITAL LETTER JHEH
        '\u{054C}' => &['\u{057C}'], // ARMENIAN CAPITAL LETTER RA
        '\u{054D}' => &['\u{057D}'], // ARMENIAN CAPITAL LETTER SEH
        '\u{054E}' => &['\u{057E}'], // ARMENIAN CAPITAL LETTER VEW
        '\u{054F}' => &['\u{057F}'], // ARMENIAN CAPITAL LETTER TIWN
        '\u{0550}' => &['\u{0580}'], // ARMENIAN CAPITAL LETTER REH
        '\u{0551}' => &['\u{0581}'], // ARMENIAN CAPITAL LETTER CO
        '\u{0552}' => &['\u{0582}'], // ARMENIAN CAPITAL LETTER YIWN
        '\u{0553}' => &['\u{0583}'], // ARMENIAN CAPITAL LETTER PIWR
        '\u{0554}' => &['\u{0584}'], // ARMENIAN CAPITAL LETTER KEH
        '\u{0555}' => &['\u{0585}'], // ARMENIAN CAPITAL LETTER OH
        '\u{0556}' => &['\u{0586}'], // ARMENIAN CAPITAL LETTER FEH
        '\u{0587}' => &['\u{0565}', '\u{0582}'], // ARMENIAN SMALL LIGATURE ECH YIWN
        '\u{10A0}' => &['\u{2D00}'], // GEORGIAN CAPITAL LETTER AN
        '\u{10A1}' => &['\u{2D01}'], // GEORGIAN CAPITAL LETTER BAN
        '\u{10A2}' => &['\u{2D02}'], // GEORGIAN CAPITAL LETTER GAN
        '\u{10A3}' => &['\u{2D03}'], // GEORGIAN CAPITAL LETTER DON
        '\u{10A4}' => &['\u{2D04}'], // GEORGIAN CAPITAL LETTER EN
        '\u{10A5}' => &['\u{2D05}'], // GEORGIAN CAPITAL LETTER VIN
        '\u{10A6}' => &['\u{2D06}'], // GEORGIAN CAPITAL LETTER ZEN
        '\u{10A7}' => &['\u{2D07}'], // GEORGIAN CAPITAL LETTER TAN
        '\u{10A8}' => &['\u{2D08}'], // GEORGIAN CAPITAL LETTER IN
        '\u{10A9}' => &['\u{2D09}'], // GEORGIAN CAPITAL LETTER KAN
        '\u{10AA}' => &['\u{2D0A}'], // GEORGIAN CAPITAL LETTER LAS
        '\u{10AB}' => &['\u{2D0B}'], // GEORGIAN CAPITAL LETTER MAN
        '\u{10AC}' => &['\u{2D0C}'], // GEORGIAN CAPITAL LETTER NAR
        '\u{10AD}' => &['\u{2D0D}'], // GEORGIAN CAPITAL LETTER ON
        '\u{10AE}' => &['\u{2D0E}'], // GEORGIAN CAPITAL LETTER PAR
        '\u{10AF}' => &['\u{2D0F}'], // GEORGIAN CAPITAL LETTER ZHAR
        '\u{10B0}' => &['\u{2D10}'], // GEORGIAN CAPITAL LETTER RAE
        '\u{10B1}' => &['\u{2D11}'], // GEORGIAN CAPITAL LETTER SAN
        '\u{10B2}' => &['\u{2D12}'], // GEORGIAN CAPITAL LETTER TAR
        '\u{10B3}' => &['\u{2D13}'], // GEORGIAN CAPITAL LETTER UN
        '\u{10B4}' => &['\u{2D14}'], // GEORGIAN CAPITAL LETTER PHAR
        '\u{10B5}' => &['\u{2D15}'], // GEORGIAN CAPITAL LETTER KHAR
        '\u{10B6}' => &['\u{2D16}'], // GEORGIAN CAPITAL LETTER GHAN
        '\u{10B7}' => &['\u{2D17}'], // GEORGIAN CAPITAL LETTER QAR
        '\u{10B8}' => &['\u{2D18}'], // GEORGIAN CAPITAL LETTER SHIN
        '\u{10B9}' => &['\u{2D19}'], // GEORGIAN CAPITAL LETTER CHIN
        '\u{10BA}' => &['\u{2D1A}'], // GEORGIAN CAPITAL LETTER CAN
        '\u{10BB}' => &['\u{2D1B}'], // GEORGIAN CAPITAL LETTER JIL
        '\u{10BC}' => &['\u{2D1C}'], // GEORGIAN CAPITAL LETTER CIL
        '\u{10BD}' => &['\u{2D1D}'], // GEORGIAN CAPITAL LETTER CHAR
        '\u{10BE}' => &['\u{2D1E}'], // GEORGIAN CAPITAL LETTER XAN
        '\u{10BF}' => &['\u{2D1F}'], // GEORGIAN CAPITAL LETTER JHAN
        '\u{10C0}' => &['\u{2D20}'], // GEORGIAN CAPITAL LETTER HAE
        '\u{10C1}' => &['\u{2D21}'], // GEORGIAN CAPITAL LETTER HE
        '\u{10C2}' => &['\u{2D22}'], // GEORGIAN CAPITAL LETTER HIE
        '\u{10C3}' => &['\u{2D23}'], // GEORGIAN CAPITAL LETTER WE
        '\u{10C4}' => &['\u{2D24}'], // GEORGIAN CAPITAL LETTER HAR
        '\u{10C5}' => &['\u{2D25}'], // GEORGIAN CAPITAL LETTER HOE
        '\u{10C7}' => &['\u{2D27}'], // GEORGIAN CAPITAL LETTER YN
        '\u{10CD}' => &['\u{2D2D}'], // GEORGIAN CAPITAL LETTER AEN
        '\u{13F8}' => &['\u{13F0}'], // CHEROKEE SMALL LETTER YE
        '\u{13F9}' => &['\u{13F1}'], // CHEROKEE SMALL LETTER YI
        '\u{13FA}' => &['\u{13F2}'], // CHEROKEE SMALL LETTER YO
        '\u{13FB}' => &['\u{13F3}'], // CHEROKEE SMALL LETTER YU
        '\u{13FC}' => &['\u{13F4}'], // CHEROKEE SMALL LETTER YV
        '\u{13FD}' => &['\u{13F5}'], // CHEROKEE SMALL LETTER MV
        '\u{1C80}' => &['\u{0432}'], // CYRILLIC SMALL LETTER ROUNDED VE
        '\u{1C81}' => &['\u{0434}'], // CYRILLIC SMALL LETTER LONG-LEGGED DE
        '\u{1C82}' => &['\u{043E}'], // CYRILLIC SMALL LETTER NARROW O
        '\u{1C83}' => &['\u{0441}'], // CYRILLIC SMALL LETTER WIDE ES
        '\u{1C84}' => &['\u{0442}'], // CYRILLIC SMALL LETTER TALL TE
        '\u{1C85}' => &['\u{0442}'], // CYRILLIC SMALL LETTER THREE-LEGGED TE
        '\u{1C86}' => &['\u{044A}'], // CYRILLIC SMALL LETTER TALL HARD SIGN
        '\u{1C87}' => &['\u{0463}'], // CYRILLIC SMALL LETTER TALL YAT
        '\u{1C88}' => &['\u{A64B}'], // CYRILLIC SMALL LETTER UNBLENDED UK
        '\u{1C89}' => &['\u{1C8A}'], // CYRILLIC CAPITAL LETTER TJE
        '\u{1C90}' => &['\u{10D0}'], // GEORGIAN MTAVRULI CAPITAL LETTER AN
        '\u{1C91}' => &['\u{10D1}'], // GEORGIAN MTAVRULI CAPITAL LETTER BAN
        '\u{1C92}' => &['\u{10D2}'], // GEORGIAN MTAVRULI CAPITAL LETTER GAN
        '\u{1C93}' => &['\u{10D3}'], // GEORGIAN MTAVRULI CAPITAL LETTER DON
        '\u{1C94}' => &['\u{10D4}'], // GEORGIAN MTAVRULI CAPITAL LETTER EN
        '\u{1C95}' => &['\u{10D5}'], // GEORGIAN MTAVRULI CAPITAL LETTER VIN
        '\u{1C96}' => &['\u{10D6}'], // GEORGIAN MTAVRULI CAPITAL LETTER ZEN
        '\u{1C97}' => &['\u{10D7}'], // GEORGIAN MTAVRULI CAPITAL LETTER TAN
        '\u{1C98}' => &['\u{10D8}'], // GEORGIAN MTAVRULI CAPITAL LETTER IN
        '\u{1C99}' => &['\u{10D9}'], // GEORGIAN MTAVRULI CAPITAL LETTER KAN
        '\u{1C9A}' => &['\u{10DA}'], // GEORGIAN MTAVRULI CAPITAL LETTER LAS
        '\u{1C9B}' => &['\u{10DB}'], // GEORGIAN MTAVRULI CAPITAL LETTER MAN
        '\u{1C9C}' => &['\u{10DC}'], // GEORGIAN MTAVRULI CAPITAL LETTER NAR
        '\u{1C9D}' => &['\u{10DD}'], // GEORGIAN MTAVRULI CAPITAL LETTER ON
        '\u{1C9E}' => &['\u{10DE}'], // GEORGIAN MTAVRULI CAPITAL LETTER PAR
        '\u{1C9F}' => &['\u{10DF}'], // GEORGIAN MTAVRULI CAPITAL LETTER ZHAR
        '\u{1CA0}' => &['\u{10E0}'], // GEORGIAN MTAVRULI CAPITAL LETTER RAE
        '\u{1CA1}' => &['\u{10E1}'], // GEORGIAN MTAVRULI CAPITAL LETTER SAN
        '\u{1CA2}' => &['\u{10E2}'], // GEORGIAN MTAVRULI CAPITAL LETTER TAR
        '\u{1CA3}' => &['\u{10E3}'], // GEORGIAN MTAVRULI CAPITAL LETTER UN
        '\u{1CA4}' => &['\u{10E4}'], // GEORGIAN MTAVRULI CAPITAL LETTER PHAR
        '\u{1CA5}' => &['\u{10E5}'], // GEORGIAN MTAVRULI CAPITAL LETTER KHAR
        '\u{1CA6}' => &['\u{10E6}'], // GEORGIAN MTAVRULI CAPITAL LETTER GHAN
        '\u{1CA7}' => &['\u{10E7}'], // GEORGIAN MTAVRULI CAPITAL LETTER QAR
        '\u{1CA8}' => &['\u{10E8}'], // GEORGIAN MTAVRULI CAPITAL LETTER SHIN
        '\u{1CA9}' => &['\u{10E9}'], // GEORGIAN MTAVRULI CAPITAL LETTER CHIN
        '\u{1CAA}' => &['\u{10EA}'], // GEORGIAN MTAVRULI CAPITAL LETTER CAN
        '\u{1CAB}' => &['\u{10EB}'], // GEORGIAN MTAVRULI CAPITAL LETTER JIL
        '\u{1CAC}' => &['\u{10EC}'], // GEORGIAN MTAVRULI CAPITAL LETTER CIL
        '\u{1CAD}' => &['\u{10ED}'], // GEORGIAN MTAVRULI CAPITAL LETTER CHAR
        '\u{1CAE}' => &['\u{10EE}'], // GEORGIAN MTAVRULI CAPITAL LETTER XAN
        '\u{1CAF}' => &['\u{10EF}'], // GEORGIAN MTAVRULI CAPITAL LETTER JHAN
        '\u{1CB0}' => &['\u{10F0}'], // GEORGIAN MTAVRULI CAPITAL LETTER HAE
        '\u{1CB1}' => &['\u{10F1}'], // GEORGIAN MTAVRULI CAPITAL LETTER HE
        '\u{1CB2}' => &['\u{10F2}'], // GEORGIAN MTAVRULI CAPITAL LETTER HIE
        '\u{1CB3}' => &['\u{10F3}'], // GEORGIAN MTAVRULI CAPITAL LETTER WE
        '\u{1CB4}' => &['\u{10F4}'], // GEORGIAN MTAVRULI CAPITAL LETTER HAR
        '\u{1CB5}' => &['\u{10F5}'], // GEORGIAN MTAVRULI CAPITAL LETTER HOE
        '\u{1CB6}' => &['\u{10F6}'], // GEORGIAN MTAVRULI CAPITAL LETTER FI
        '\u{1CB7}' => &['\u{10F7}'], // GEORGIAN MTAVRULI CAPITAL LETTER YN
        '\u{1CB8}' => &['\u{10F8}'], // GEORGIAN MTAVRULI CAPITAL LETTER ELIFI
        '\u{1CB9}' => &['\u{10F9}'], // GEORGIAN MTAVRULI CAPITAL LETTER TURNED GAN
        '\u{1CBA}' => &['\u{10FA}'], // GEORGIAN MTAVRULI CAPITAL LETTER AIN
        '\u{1CBD}' => &['\u{10FD}'], // GEORGIAN MTAVRULI CAPITAL LETTER AEN
        '\u{1CBE}' => &['\u{10FE}'], // GEORGIAN MTAVRULI CAPITAL LETTER HARD SIGN
        '\u{1CBF}' => &['\u{10FF}'], // GEORGIAN MTAVRULI CAPITAL LETTER LABIAL SIGN
        '\u{1E00}' => &['\u{1E01}'], // LATIN CAPITAL LETTER A WITH RING BELOW
        '\u{1E02}' => &['\u{1E03}'], // LATIN CAPITAL LETTER B WITH DOT ABOVE
        '\u{1E04}' => &['\u{1E05}'], // LATIN CAPITAL LETTER B WITH DOT BELOW
        '\u{1E06}' => &['\u{1E07}'], // LATIN CAPITAL LETTER B WITH LINE BELOW
        '\u{1E08}' => &['\u{1E09}'], // LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
        '\u{1E0A}' => &['\u{1E0B}'], // LATIN CAPITAL LETTER D WITH DOT ABOVE
        '\u{1E0C}' => &['\u{1E0D}'], // LATIN CAPITAL LETTER D WITH DOT BELOW
        '\u{1E0E}' => &['\u{1E0F}'], // LATIN CAPITAL LETTER D WITH LINE BELOW
        '\u{1E10}' => &['\u{1E11}'], // LATIN CAPITAL LETTER D WITH CEDILLA
        '\u{1E12}' => &['\u{1E13}'], // LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
        '\u{1E14}' => &['\u{1E15}'], // LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
        '\u{1E16}' => &['\u{1E17}'], // LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
        '\u{1E18}' => &['\u{1E19}'], // LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
        '\u{1E1A}' => &['\u{1E1B}'], // LATIN CAPITAL LETTER E WITH TILDE BELOW
        '\u{1E1C}' => &['\u{1E1D}'], // LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
        '\u{1E1E}' => &['\u{1E1F}'], // LATIN CAPITAL LETTER F WITH DOT ABOVE
        '\u{1E20}' => &['\u{1E21}'], // LATIN CAPITAL LETTER G WITH MACRON
        '\u{1E22}' => &['\u{1E23}'], // LATIN CAPITAL LETTER H WITH DOT ABOVE
        '\u{1E24}' => &['\u{1E25}'], // LATIN CAPITAL LETTER H WITH DOT BELOW
        '\u{1E26}' => &['\u{1E27}'], // LATIN CAPITAL LETTER H WITH DIAERESIS
        '\u{1E28}' => &['\u{1E29}'], // LATIN CAPITAL LETTER H WITH CEDILLA
        '\u{1E2A}' => &['\u{1E2B}'], // LATIN CAPITAL LETTER H WITH BREVE BELOW
        '\u{1E2C}' => &['\u{1E2D}'], // LATIN CAPITAL LETTER I WITH TILDE BELOW
        '\u{1E2E}' => &['\u{1E2F}'], // LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
        '\u{1E30}' => &['\u{1E31}'], // LATIN CAPITAL LETTER K WITH ACUTE
        '\u{1E32}' => &['\u{1E33}'], // LATIN CAPITAL LETTER K WITH DOT BELOW
        '\u{1E34}' => &['\u{1E35}'], // LATIN CAPITAL LETTER K WITH LINE BELOW
        '\u{1E36}' => &['\u{1E37}'], // LATIN CAPITAL LETTER L WITH DOT BELOW
        '\u{1E38}' => &['\u{1E39}'], // LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
        '\u{1E3A}' => &['\u{1E3B}'], // LATIN CAPITAL LETTER L WITH LINE BELOW
        '\u{1E3C}' => &['\u{1E3D}'], // LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
        '\u{1E3E}' => &['\u{1E3F}'], // LATIN CAPITAL LETTER M WITH ACUTE
        '\u{1E40}' => &['\u{1E41}'], // LATIN CAPITAL LETTER M WITH DOT ABOVE
        '\u{1E42}' => &['\u{1E43}'], // LATIN CAPITAL LETTER M WITH DOT BELOW
        '\u{1E44}' => &['\u{1E45}'], // LATIN CAPITAL LETTER N WITH DOT ABOVE
        '\u{1E46}' => &['\u{1E47}'], // LATIN CAPITAL LETTER N WITH DOT BELOW
        '\u{1E48}' => &['\u{1E49}'], // LATIN CAPITAL LETTER N WITH LINE BELOW
        '\u{1E4A}' => &['\u{1E4B}'], // LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
        '\u{1E4C}' => &['\u{1E4D}'], // LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
        '\u{1E4E}' => &['\u{1E4F}'], // LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
        '\u{1E50}' => &['\u{1E51}'], // LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
        '\u{1E52}' => &['\u{1E53}'], // LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
        '\u{1E54}' => &['\u{1E55}'], // LATIN CAPITAL LETTER P WITH ACUTE
        '\u{1E56}' => &['\u{1E57}'], // LATIN CAPITAL LETTER P WITH DOT ABOVE
        '\u{1E58}' => &['\u{1E59}'], // LATIN CAPITAL LETTER R WITH DOT ABOVE
        '\u{1E5A}' => &['\u{1E5B}'], // LATIN CAPITAL LETTER R WITH DOT BELOW
        '\u{1E5C}' => &['\u{1E5D}'], // LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
        '\u{1E5E}' => &['\u{1E5F}'], // LATIN CAPITAL LETTER R WITH LINE BELOW
        '\u{1E60}' => &['\u{1E61}'], // LATIN CAPITAL LETTER S WITH DOT ABOVE
        '\u{1E62}' => &['\u{1E63}'], // LATIN CAPITAL LETTER S WITH DOT BELOW
        '\u{1E64}' => &['\u{1E65}'], // LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
        '\u{1E66}' => &['\u{1E67}'], // LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
        '\u{1E68}' => &['\u{1E69}'], // LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
        '\u{1E6A}' => &['\u{1E6B}'], // LATIN CAPITAL LETTER T WITH DOT ABOVE
        '\u{1E6C}' => &['\u{1E6D}'], // LATIN CAPITAL LETTER T WITH DOT BELOW
        '\u{1E6E}' => &['\u{1E6F}'], // LATIN CAPITAL LETTER T WITH LINE BELOW
        '\u{1E70}' => &['\u{1E71}'], // LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
        '\u{1E72}' => &['\u{1E73}'], // LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
        '\u{1E74}' => &['\u{1E75}'], // LATIN CAPITAL LETTER U WITH TILDE BELOW
        '\u{1E76}' => &['\u{1E77}'], // LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
        '\u{1E78}' => &['\u{1E79}'], // LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
        '\u{1E7A}' => &['\u{1E7B}'], // LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
        '\u{1E7C}' => &['\u{1E7D}'], // LATIN CAPITAL LETTER V WITH TILDE
        '\u{1E7E}' => &['\u{1E7F}'], // LATIN CAPITAL LETTER V WITH DOT BELOW
        '\u{1E80}' => &['\u{1E81}'], // LATIN CAPITAL LETTER W WITH GRAVE
        '\u{1E82}' => &['\u{1E83}'], // LATIN CAPITAL LETTER W WITH ACUTE
        '\u{1E84}' => &['\u{1E85}'], // LATIN CAPITAL LETTER W WITH DIAERESIS
        '\u{1E86}' => &['\u{1E87}'], // LATIN CAPITAL LETTER W WITH DOT ABOVE
        '\u{1E88}' => &['\u{1E89}'], // LATIN CAPITAL LETTER W WITH DOT BELOW
        '\u{1E8A}' => &['\u{1E8B}'], // LATIN CAPITAL LETTER X WITH DOT ABOVE
        '\u{1E8C}' => &['\u{1E8D}'], // LATIN CAPITAL LETTER X WITH DIAERESIS
        '\u{1E8E}' => &['\u{1E8F}'], // LATIN CAPITAL LETTER Y WITH DOT ABOVE
        '\u{1E90}' => &['\u{1E91}'], // LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
        '\u{1E92}' => &['\u{1E93}'], // LATIN CAPITAL LETTER Z WITH DOT BELOW
        '\u{1E94}' => &['\u{1E95}'], // LATIN CAPITAL LETTER Z WITH LINE BELOW
        '\u{1E96}' => &['\u{0068}', '\u{0331}'], // LATIN SMALL LETTER H WITH LINE BELOW
        '\u{1E97}' => &['\u{0074}', '\u{0308}'], // LATIN SMALL LETTER T WITH DIAERESIS
        '\u{1E98}' => &['\u{0077}', '\u{030A}'], // LATIN SMALL LETTER W WITH RING ABOVE
        '\u{1E99}' => &['\u{0079}', '\u{030A}'], // LATIN SMALL LETTER Y WITH RING ABOVE
        '\u{1E9A}' => &['\u{0061}', '\u{02BE}'], // LATIN SMALL LETTER A WITH RIGHT HALF RING
        '\u{1E9B}' => &['\u{1E61}'], // LATIN SMALL LETTER LONG S WITH DOT ABOVE
        // '\u{1E9E}' => &['\u{0073}', '\u{0073}'], // LATIN CAPITAL LETTER SHARP S
        '\u{1E9E}' => &['\u{00DF}'], // LATIN CAPITAL LETTER SHARP S
        '\u{1EA0}' => &['\u{1EA1}'], // LATIN CAPITAL LETTER A WITH DOT BELOW
        '\u{1EA2}' => &['\u{1EA3}'], // LATIN CAPITAL LETTER A WITH HOOK ABOVE
        '\u{1EA4}' => &['\u{1EA5}'], // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
        '\u{1EA6}' => &['\u{1EA7}'], // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
        '\u{1EA8}' => &['\u{1EA9}'], // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
        '\u{1EAA}' => &['\u{1EAB}'], // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
        '\u{1EAC}' => &['\u{1EAD}'], // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
        '\u{1EAE}' => &['\u{1EAF}'], // LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
        '\u{1EB0}' => &['\u{1EB1}'], // LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
        '\u{1EB2}' => &['\u{1EB3}'], // LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
        '\u{1EB4}' => &['\u{1EB5}'], // LATIN CAPITAL LETTER A WITH BREVE AND TILDE
        '\u{1EB6}' => &['\u{1EB7}'], // LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
        '\u{1EB8}' => &['\u{1EB9}'], // LATIN CAPITAL LETTER E WITH DOT BELOW
        '\u{1EBA}' => &['\u{1EBB}'], // LATIN CAPITAL LETTER E WITH HOOK ABOVE
        '\u{1EBC}' => &['\u{1EBD}'], // LATIN CAPITAL LETTER E WITH TILDE
        '\u{1EBE}' => &['\u{1EBF}'], // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
        '\u{1EC0}' => &['\u{1EC1}'], // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
        '\u{1EC2}' => &['\u{1EC3}'], // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
        '\u{1EC4}' => &['\u{1EC5}'], // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
        '\u{1EC6}' => &['\u{1EC7}'], // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
        '\u{1EC8}' => &['\u{1EC9}'], // LATIN CAPITAL LETTER I WITH HOOK ABOVE
        '\u{1ECA}' => &['\u{1ECB}'], // LATIN CAPITAL LETTER I WITH DOT BELOW
        '\u{1ECC}' => &['\u{1ECD}'], // LATIN CAPITAL LETTER O WITH DOT BELOW
        '\u{1ECE}' => &['\u{1ECF}'], // LATIN CAPITAL LETTER O WITH HOOK ABOVE
        '\u{1ED0}' => &['\u{1ED1}'], // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
        '\u{1ED2}' => &['\u{1ED3}'], // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
        '\u{1ED4}' => &['\u{1ED5}'], // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
        '\u{1ED6}' => &['\u{1ED7}'], // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
        '\u{1ED8}' => &['\u{1ED9}'], // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
        '\u{1EDA}' => &['\u{1EDB}'], // LATIN CAPITAL LETTER O WITH HORN AND ACUTE
        '\u{1EDC}' => &['\u{1EDD}'], // LATIN CAPITAL LETTER O WITH HORN AND GRAVE
        '\u{1EDE}' => &['\u{1EDF}'], // LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
        '\u{1EE0}' => &['\u{1EE1}'], // LATIN CAPITAL LETTER O WITH HORN AND TILDE
        '\u{1EE2}' => &['\u{1EE3}'], // LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
        '\u{1EE4}' => &['\u{1EE5}'], // LATIN CAPITAL LETTER U WITH DOT BELOW
        '\u{1EE6}' => &['\u{1EE7}'], // LATIN CAPITAL LETTER U WITH HOOK ABOVE
        '\u{1EE8}' => &['\u{1EE9}'], // LATIN CAPITAL LETTER U WITH HORN AND ACUTE
        '\u{1EEA}' => &['\u{1EEB}'], // LATIN CAPITAL LETTER U WITH HORN AND GRAVE
        '\u{1EEC}' => &['\u{1EED}'], // LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
        '\u{1EEE}' => &['\u{1EEF}'], // LATIN CAPITAL LETTER U WITH HORN AND TILDE
        '\u{1EF0}' => &['\u{1EF1}'], // LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
        '\u{1EF2}' => &['\u{1EF3}'], // LATIN CAPITAL LETTER Y WITH GRAVE
        '\u{1EF4}' => &['\u{1EF5}'], // LATIN CAPITAL LETTER Y WITH DOT BELOW
        '\u{1EF6}' => &['\u{1EF7}'], // LATIN CAPITAL LETTER Y WITH HOOK ABOVE
        '\u{1EF8}' => &['\u{1EF9}'], // LATIN CAPITAL LETTER Y WITH TILDE
        '\u{1EFA}' => &['\u{1EFB}'], // LATIN CAPITAL LETTER MIDDLE-WELSH LL
        '\u{1EFC}' => &['\u{1EFD}'], // LATIN CAPITAL LETTER MIDDLE-WELSH V
        '\u{1EFE}' => &['\u{1EFF}'], // LATIN CAPITAL LETTER Y WITH LOOP
        '\u{1F08}' => &['\u{1F00}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI
        '\u{1F09}' => &['\u{1F01}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA
        '\u{1F0A}' => &['\u{1F02}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
        '\u{1F0B}' => &['\u{1F03}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
        '\u{1F0C}' => &['\u{1F04}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
        '\u{1F0D}' => &['\u{1F05}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
        '\u{1F0E}' => &['\u{1F06}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
        '\u{1F0F}' => &['\u{1F07}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
        '\u{1F18}' => &['\u{1F10}'], // GREEK CAPITAL LETTER EPSILON WITH PSILI
        '\u{1F19}' => &['\u{1F11}'], // GREEK CAPITAL LETTER EPSILON WITH DASIA
        '\u{1F1A}' => &['\u{1F12}'], // GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
        '\u{1F1B}' => &['\u{1F13}'], // GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
        '\u{1F1C}' => &['\u{1F14}'], // GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
        '\u{1F1D}' => &['\u{1F15}'], // GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
        '\u{1F28}' => &['\u{1F20}'], // GREEK CAPITAL LETTER ETA WITH PSILI
        '\u{1F29}' => &['\u{1F21}'], // GREEK CAPITAL LETTER ETA WITH DASIA
        '\u{1F2A}' => &['\u{1F22}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
        '\u{1F2B}' => &['\u{1F23}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
        '\u{1F2C}' => &['\u{1F24}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
        '\u{1F2D}' => &['\u{1F25}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
        '\u{1F2E}' => &['\u{1F26}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
        '\u{1F2F}' => &['\u{1F27}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
        '\u{1F38}' => &['\u{1F30}'], // GREEK CAPITAL LETTER IOTA WITH PSILI
        '\u{1F39}' => &['\u{1F31}'], // GREEK CAPITAL LETTER IOTA WITH DASIA
        '\u{1F3A}' => &['\u{1F32}'], // GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
        '\u{1F3B}' => &['\u{1F33}'], // GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
        '\u{1F3C}' => &['\u{1F34}'], // GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
        '\u{1F3D}' => &['\u{1F35}'], // GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
        '\u{1F3E}' => &['\u{1F36}'], // GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
        '\u{1F3F}' => &['\u{1F37}'], // GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
        '\u{1F48}' => &['\u{1F40}'], // GREEK CAPITAL LETTER OMICRON WITH PSILI
        '\u{1F49}' => &['\u{1F41}'], // GREEK CAPITAL LETTER OMICRON WITH DASIA
        '\u{1F4A}' => &['\u{1F42}'], // GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
        '\u{1F4B}' => &['\u{1F43}'], // GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
        '\u{1F4C}' => &['\u{1F44}'], // GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
        '\u{1F4D}' => &['\u{1F45}'], // GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
        '\u{1F50}' => &['\u{03C5}', '\u{0313}'], // GREEK SMALL LETTER UPSILON WITH PSILI
        '\u{1F52}' => &['\u{03C5}', '\u{0313}', '\u{0300}'], // GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
        '\u{1F54}' => &['\u{03C5}', '\u{0313}', '\u{0301}'], // GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
        '\u{1F56}' => &['\u{03C5}', '\u{0313}', '\u{0342}'], // GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
        '\u{1F59}' => &['\u{1F51}'], // GREEK CAPITAL LETTER UPSILON WITH DASIA
        '\u{1F5B}' => &['\u{1F53}'], // GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
        '\u{1F5D}' => &['\u{1F55}'], // GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
        '\u{1F5F}' => &['\u{1F57}'], // GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
        '\u{1F68}' => &['\u{1F60}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI
        '\u{1F69}' => &['\u{1F61}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA
        '\u{1F6A}' => &['\u{1F62}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
        '\u{1F6B}' => &['\u{1F63}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
        '\u{1F6C}' => &['\u{1F64}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
        '\u{1F6D}' => &['\u{1F65}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
        '\u{1F6E}' => &['\u{1F66}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
        '\u{1F6F}' => &['\u{1F67}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
        '\u{1F80}' => &['\u{1F00}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
        '\u{1F81}' => &['\u{1F01}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
        '\u{1F82}' => &['\u{1F02}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
        '\u{1F83}' => &['\u{1F03}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
        '\u{1F84}' => &['\u{1F04}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
        '\u{1F85}' => &['\u{1F05}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
        '\u{1F86}' => &['\u{1F06}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
        '\u{1F87}' => &['\u{1F07}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
        // '\u{1F88}' => &['\u{1F00}', '\u{03B9}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
        '\u{1F88}' => &['\u{1F80}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
        // '\u{1F89}' => &['\u{1F01}', '\u{03B9}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
        '\u{1F89}' => &['\u{1F81}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
        // '\u{1F8A}' => &['\u{1F02}', '\u{03B9}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
        '\u{1F8A}' => &['\u{1F82}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
        // '\u{1F8B}' => &['\u{1F03}', '\u{03B9}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
        '\u{1F8B}' => &['\u{1F83}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
        // '\u{1F8C}' => &['\u{1F04}', '\u{03B9}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
        '\u{1F8C}' => &['\u{1F84}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
        // '\u{1F8D}' => &['\u{1F05}', '\u{03B9}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
        '\u{1F8D}' => &['\u{1F85}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
        // '\u{1F8E}' => &['\u{1F06}', '\u{03B9}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
        '\u{1F8E}' => &['\u{1F86}'], // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
        // '\u{1F8F}' => &['\u{1F07}', '\u{03B9}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
        '\u{1F8F}' => &['\u{1F87}'], // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
        '\u{1F90}' => &['\u{1F20}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
        '\u{1F91}' => &['\u{1F21}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
        '\u{1F92}' => &['\u{1F22}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
        '\u{1F93}' => &['\u{1F23}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
        '\u{1F94}' => &['\u{1F24}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
        '\u{1F95}' => &['\u{1F25}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
        '\u{1F96}' => &['\u{1F26}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
        '\u{1F97}' => &['\u{1F27}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
        // '\u{1F98}' => &['\u{1F20}', '\u{03B9}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
        '\u{1F98}' => &['\u{1F90}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
        // '\u{1F99}' => &['\u{1F21}', '\u{03B9}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
        '\u{1F99}' => &['\u{1F91}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
        // '\u{1F9A}' => &['\u{1F22}', '\u{03B9}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
        '\u{1F9A}' => &['\u{1F92}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
        // '\u{1F9B}' => &['\u{1F23}', '\u{03B9}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
        '\u{1F9B}' => &['\u{1F93}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
        // '\u{1F9C}' => &['\u{1F24}', '\u{03B9}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
        '\u{1F9C}' => &['\u{1F94}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
        // '\u{1F9D}' => &['\u{1F25}', '\u{03B9}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
        '\u{1F9D}' => &['\u{1F95}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
        // '\u{1F9E}' => &['\u{1F26}', '\u{03B9}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
        '\u{1F9E}' => &['\u{1F96}'], // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
        // '\u{1F9F}' => &['\u{1F27}', '\u{03B9}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
        '\u{1F9F}' => &['\u{1F97}'], // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
        '\u{1FA0}' => &['\u{1F60}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
        '\u{1FA1}' => &['\u{1F61}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
        '\u{1FA2}' => &['\u{1F62}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
        '\u{1FA3}' => &['\u{1F63}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
        '\u{1FA4}' => &['\u{1F64}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
        '\u{1FA5}' => &['\u{1F65}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
        '\u{1FA6}' => &['\u{1F66}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
        '\u{1FA7}' => &['\u{1F67}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
        // '\u{1FA8}' => &['\u{1F60}', '\u{03B9}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
        '\u{1FA8}' => &['\u{1FA0}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
        // '\u{1FA9}' => &['\u{1F61}', '\u{03B9}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
        '\u{1FA9}' => &['\u{1FA1}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
        // '\u{1FAA}' => &['\u{1F62}', '\u{03B9}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
        '\u{1FAA}' => &['\u{1FA2}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
        // '\u{1FAB}' => &['\u{1F63}', '\u{03B9}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
        '\u{1FAB}' => &['\u{1FA3}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
        // '\u{1FAC}' => &['\u{1F64}', '\u{03B9}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
        '\u{1FAC}' => &['\u{1FA4}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
        // '\u{1FAD}' => &['\u{1F65}', '\u{03B9}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
        '\u{1FAD}' => &['\u{1FA5}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
        // '\u{1FAE}' => &['\u{1F66}', '\u{03B9}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
        '\u{1FAE}' => &['\u{1FA6}'], // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
        // '\u{1FAF}' => &['\u{1F67}', '\u{03B9}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
        '\u{1FAF}' => &['\u{1FA7}'], // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
        '\u{1FB2}' => &['\u{1F70}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
        '\u{1FB3}' => &['\u{03B1}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
        '\u{1FB4}' => &['\u{03AC}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
        '\u{1FB6}' => &['\u{03B1}', '\u{0342}'], // GREEK SMALL LETTER ALPHA WITH PERISPOMENI
        '\u{1FB7}' => &['\u{03B1}', '\u{0342}', '\u{03B9}'], // GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
        '\u{1FB8}' => &['\u{1FB0}'], // GREEK CAPITAL LETTER ALPHA WITH VRACHY
        '\u{1FB9}' => &['\u{1FB1}'], // GREEK CAPITAL LETTER ALPHA WITH MACRON
        '\u{1FBA}' => &['\u{1F70}'], // GREEK CAPITAL LETTER ALPHA WITH VARIA
        '\u{1FBB}' => &['\u{1F71}'], // GREEK CAPITAL LETTER ALPHA WITH OXIA
        // '\u{1FBC}' => &['\u{03B1}', '\u{03B9}'], // GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
        '\u{1FBC}' => &['\u{1FB3}'], // GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
        '\u{1FBE}' => &['\u{03B9}'], // GREEK PROSGEGRAMMENI
        '\u{1FC2}' => &['\u{1F74}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
        '\u{1FC3}' => &['\u{03B7}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
        '\u{1FC4}' => &['\u{03AE}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
        '\u{1FC6}' => &['\u{03B7}', '\u{0342}'], // GREEK SMALL LETTER ETA WITH PERISPOMENI
        '\u{1FC7}' => &['\u{03B7}', '\u{0342}', '\u{03B9}'], // GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
        '\u{1FC8}' => &['\u{1F72}'], // GREEK CAPITAL LETTER EPSILON WITH VARIA
        '\u{1FC9}' => &['\u{1F73}'], // GREEK CAPITAL LETTER EPSILON WITH OXIA
        '\u{1FCA}' => &['\u{1F74}'], // GREEK CAPITAL LETTER ETA WITH VARIA
        '\u{1FCB}' => &['\u{1F75}'], // GREEK CAPITAL LETTER ETA WITH OXIA
        // '\u{1FCC}' => &['\u{03B7}', '\u{03B9}'], // GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
        '\u{1FCC}' => &['\u{1FC3}'], // GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
        '\u{1FD2}' => &['\u{03B9}', '\u{0308}', '\u{0300}'], // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
        // '\u{1FD3}' => &['\u{03B9}', '\u{0308}', '\u{0301}'], // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
        '\u{1FD3}' => &['\u{0390}'], // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
        '\u{1FD6}' => &['\u{03B9}', '\u{0342}'], // GREEK SMALL LETTER IOTA WITH PERISPOMENI
        '\u{1FD7}' => &['\u{03B9}', '\u{0308}', '\u{0342}'], // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
        '\u{1FD8}' => &['\u{1FD0}'], // GREEK CAPITAL LETTER IOTA WITH VRACHY
        '\u{1FD9}' => &['\u{1FD1}'], // GREEK CAPITAL LETTER IOTA WITH MACRON
        '\u{1FDA}' => &['\u{1F76}'], // GREEK CAPITAL LETTER IOTA WITH VARIA
        '\u{1FDB}' => &['\u{1F77}'], // GREEK CAPITAL LETTER IOTA WITH OXIA
        '\u{1FE2}' => &['\u{03C5}', '\u{0308}', '\u{0300}'], // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
        // '\u{1FE3}' => &['\u{03C5}', '\u{0308}', '\u{0301}'], // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
        '\u{1FE3}' => &['\u{03B0}'], // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
        '\u{1FE4}' => &['\u{03C1}', '\u{0313}'], // GREEK SMALL LETTER RHO WITH PSILI
        '\u{1FE6}' => &['\u{03C5}', '\u{0342}'], // GREEK SMALL LETTER UPSILON WITH PERISPOMENI
        '\u{1FE7}' => &['\u{03C5}', '\u{0308}', '\u{0342}'], // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
        '\u{1FE8}' => &['\u{1FE0}'], // GREEK CAPITAL LETTER UPSILON WITH VRACHY
        '\u{1FE9}' => &['\u{1FE1}'], // GREEK CAPITAL LETTER UPSILON WITH MACRON
        '\u{1FEA}' => &['\u{1F7A}'], // GREEK CAPITAL LETTER UPSILON WITH VARIA
        '\u{1FEB}' => &['\u{1F7B}'], // GREEK CAPITAL LETTER UPSILON WITH OXIA
        '\u{1FEC}' => &['\u{1FE5}'], // GREEK CAPITAL LETTER RHO WITH DASIA
        '\u{1FF2}' => &['\u{1F7C}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
        '\u{1FF3}' => &['\u{03C9}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
        '\u{1FF4}' => &['\u{03CE}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
        '\u{1FF6}' => &['\u{03C9}', '\u{0342}'], // GREEK SMALL LETTER OMEGA WITH PERISPOMENI
        '\u{1FF7}' => &['\u{03C9}', '\u{0342}', '\u{03B9}'], // GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
        '\u{1FF8}' => &['\u{1F78}'], // GREEK CAPITAL LETTER OMICRON WITH VARIA
        '\u{1FF9}' => &['\u{1F79}'], // GREEK CAPITAL LETTER OMICRON WITH OXIA
        '\u{1FFA}' => &['\u{1F7C}'], // GREEK CAPITAL LETTER OMEGA WITH VARIA
        '\u{1FFB}' => &['\u{1F7D}'], // GREEK CAPITAL LETTER OMEGA WITH OXIA
        // '\u{1FFC}' => &['\u{03C9}', '\u{03B9}'], // GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
        '\u{1FFC}' => &['\u{1FF3}'], // GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
        '\u{2126}' => &['\u{03C9}'], // OHM SIGN
        '\u{212A}' => &['\u{006B}'], // KELVIN SIGN
        '\u{212B}' => &['\u{00E5}'], // ANGSTROM SIGN
        '\u{2132}' => &['\u{214E}'], // TURNED CAPITAL F
        '\u{2160}' => &['\u{2170}'], // ROMAN NUMERAL ONE
        '\u{2161}' => &['\u{2171}'], // ROMAN NUMERAL TWO
        '\u{2162}' => &['\u{2172}'], // ROMAN NUMERAL THREE
        '\u{2163}' => &['\u{2173}'], // ROMAN NUMERAL FOUR
        '\u{2164}' => &['\u{2174}'], // ROMAN NUMERAL FIVE
        '\u{2165}' => &['\u{2175}'], // ROMAN NUMERAL SIX
        '\u{2166}' => &['\u{2176}'], // ROMAN NUMERAL SEVEN
        '\u{2167}' => &['\u{2177}'], // ROMAN NUMERAL EIGHT
        '\u{2168}' => &['\u{2178}'], // ROMAN NUMERAL NINE
        '\u{2169}' => &['\u{2179}'], // ROMAN NUMERAL TEN
        '\u{216A}' => &['\u{217A}'], // ROMAN NUMERAL ELEVEN
        '\u{216B}' => &['\u{217B}'], // ROMAN NUMERAL TWELVE
        '\u{216C}' => &['\u{217C}'], // ROMAN NUMERAL FIFTY
        '\u{216D}' => &['\u{217D}'], // ROMAN NUMERAL ONE HUNDRED
        '\u{216E}' => &['\u{217E}'], // ROMAN NUMERAL FIVE HUNDRED
        '\u{216F}' => &['\u{217F}'], // ROMAN NUMERAL ONE THOUSAND
        '\u{2183}' => &['\u{2184}'], // ROMAN NUMERAL REVERSED ONE HUNDRED
        '\u{24B6}' => &['\u{24D0}'], // CIRCLED LATIN CAPITAL LETTER A
        '\u{24B7}' => &['\u{24D1}'], // CIRCLED LATIN CAPITAL LETTER B
        '\u{24B8}' => &['\u{24D2}'], // CIRCLED LATIN CAPITAL LETTER C
        '\u{24B9}' => &['\u{24D3}'], // CIRCLED LATIN CAPITAL LETTER D
        '\u{24BA}' => &['\u{24D4}'], // CIRCLED LATIN CAPITAL LETTER E
        '\u{24BB}' => &['\u{24D5}'], // CIRCLED LATIN CAPITAL LETTER F
        '\u{24BC}' => &['\u{24D6}'], // CIRCLED LATIN CAPITAL LETTER G
        '\u{24BD}' => &['\u{24D7}'], // CIRCLED LATIN CAPITAL LETTER H
        '\u{24BE}' => &['\u{24D8}'], // CIRCLED LATIN CAPITAL LETTER I
        '\u{24BF}' => &['\u{24D9}'], // CIRCLED LATIN CAPITAL LETTER J
        '\u{24C0}' => &['\u{24DA}'], // CIRCLED LATIN CAPITAL LETTER K
        '\u{24C1}' => &['\u{24DB}'], // CIRCLED LATIN CAPITAL LETTER L
        '\u{24C2}' => &['\u{24DC}'], // CIRCLED LATIN CAPITAL LETTER M
        '\u{24C3}' => &['\u{24DD}'], // CIRCLED LATIN CAPITAL LETTER N
        '\u{24C4}' => &['\u{24DE}'], // CIRCLED LATIN CAPITAL LETTER O
        '\u{24C5}' => &['\u{24DF}'], // CIRCLED LATIN CAPITAL LETTER P
        '\u{24C6}' => &['\u{24E0}'], // CIRCLED LATIN CAPITAL LETTER Q
        '\u{24C7}' => &['\u{24E1}'], // CIRCLED LATIN CAPITAL LETTER R
        '\u{24C8}' => &['\u{24E2}'], // CIRCLED LATIN CAPITAL LETTER S
        '\u{24C9}' => &['\u{24E3}'], // CIRCLED LATIN CAPITAL LETTER T
        '\u{24CA}' => &['\u{24E4}'], // CIRCLED LATIN CAPITAL LETTER U
        '\u{24CB}' => &['\u{24E5}'], // CIRCLED LATIN CAPITAL LETTER V
        '\u{24CC}' => &['\u{24E6}'], // CIRCLED LATIN CAPITAL LETTER W
        '\u{24CD}' => &['\u{24E7}'], // CIRCLED LATIN CAPITAL LETTER X
        '\u{24CE}' => &['\u{24E8}'], // CIRCLED LATIN CAPITAL LETTER Y
        '\u{24CF}' => &['\u{24E9}'], // CIRCLED LATIN CAPITAL LETTER Z
        '\u{2C00}' => &['\u{2C30}'], // GLAGOLITIC CAPITAL LETTER AZU
        '\u{2C01}' => &['\u{2C31}'], // GLAGOLITIC CAPITAL LETTER BUKY
        '\u{2C02}' => &['\u{2C32}'], // GLAGOLITIC CAPITAL LETTER VEDE
        '\u{2C03}' => &['\u{2C33}'], // GLAGOLITIC CAPITAL LETTER GLAGOLI
        '\u{2C04}' => &['\u{2C34}'], // GLAGOLITIC CAPITAL LETTER DOBRO
        '\u{2C05}' => &['\u{2C35}'], // GLAGOLITIC CAPITAL LETTER YESTU
        '\u{2C06}' => &['\u{2C36}'], // GLAGOLITIC CAPITAL LETTER ZHIVETE
        '\u{2C07}' => &['\u{2C37}'], // GLAGOLITIC CAPITAL LETTER DZELO
        '\u{2C08}' => &['\u{2C38}'], // GLAGOLITIC CAPITAL LETTER ZEMLJA
        '\u{2C09}' => &['\u{2C39}'], // GLAGOLITIC CAPITAL LETTER IZHE
        '\u{2C0A}' => &['\u{2C3A}'], // GLAGOLITIC CAPITAL LETTER INITIAL IZHE
        '\u{2C0B}' => &['\u{2C3B}'], // GLAGOLITIC CAPITAL LETTER I
        '\u{2C0C}' => &['\u{2C3C}'], // GLAGOLITIC CAPITAL LETTER DJERVI
        '\u{2C0D}' => &['\u{2C3D}'], // GLAGOLITIC CAPITAL LETTER KAKO
        '\u{2C0E}' => &['\u{2C3E}'], // GLAGOLITIC CAPITAL LETTER LJUDIJE
        '\u{2C0F}' => &['\u{2C3F}'], // GLAGOLITIC CAPITAL LETTER MYSLITE
        '\u{2C10}' => &['\u{2C40}'], // GLAGOLITIC CAPITAL LETTER NASHI
        '\u{2C11}' => &['\u{2C41}'], // GLAGOLITIC CAPITAL LETTER ONU
        '\u{2C12}' => &['\u{2C42}'], // GLAGOLITIC CAPITAL LETTER POKOJI
        '\u{2C13}' => &['\u{2C43}'], // GLAGOLITIC CAPITAL LETTER RITSI
        '\u{2C14}' => &['\u{2C44}'], // GLAGOLITIC CAPITAL LETTER SLOVO
        '\u{2C15}' => &['\u{2C45}'], // GLAGOLITIC CAPITAL LETTER TVRIDO
        '\u{2C16}' => &['\u{2C46}'], // GLAGOLITIC CAPITAL LETTER UKU
        '\u{2C17}' => &['\u{2C47}'], // GLAGOLITIC CAPITAL LETTER FRITU
        '\u{2C18}' => &['\u{2C48}'], // GLAGOLITIC CAPITAL LETTER HERU
        '\u{2C19}' => &['\u{2C49}'], // GLAGOLITIC CAPITAL LETTER OTU
        '\u{2C1A}' => &['\u{2C4A}'], // GLAGOLITIC CAPITAL LETTER PE
        '\u{2C1B}' => &['\u{2C4B}'], // GLAGOLITIC CAPITAL LETTER SHTA
        '\u{2C1C}' => &['\u{2C4C}'], // GLAGOLITIC CAPITAL LETTER TSI
        '\u{2C1D}' => &['\u{2C4D}'], // GLAGOLITIC CAPITAL LETTER CHRIVI
        '\u{2C1E}' => &['\u{2C4E}'], // GLAGOLITIC CAPITAL LETTER SHA
        '\u{2C1F}' => &['\u{2C4F}'], // GLAGOLITIC CAPITAL LETTER YERU
        '\u{2C20}' => &['\u{2C50}'], // GLAGOLITIC CAPITAL LETTER YERI
        '\u{2C21}' => &['\u{2C51}'], // GLAGOLITIC CAPITAL LETTER YATI
        '\u{2C22}' => &['\u{2C52}'], // GLAGOLITIC CAPITAL LETTER SPIDERY HA
        '\u{2C23}' => &['\u{2C53}'], // GLAGOLITIC CAPITAL LETTER YU
        '\u{2C24}' => &['\u{2C54}'], // GLAGOLITIC CAPITAL LETTER SMALL YUS
        '\u{2C25}' => &['\u{2C55}'], // GLAGOLITIC CAPITAL LETTER SMALL YUS WITH TAIL
        '\u{2C26}' => &['\u{2C56}'], // GLAGOLITIC CAPITAL LETTER YO
        '\u{2C27}' => &['\u{2C57}'], // GLAGOLITIC CAPITAL LETTER IOTATED SMALL YUS
        '\u{2C28}' => &['\u{2C58}'], // GLAGOLITIC CAPITAL LETTER BIG YUS
        '\u{2C29}' => &['\u{2C59}'], // GLAGOLITIC CAPITAL LETTER IOTATED BIG YUS
        '\u{2C2A}' => &['\u{2C5A}'], // GLAGOLITIC CAPITAL LETTER FITA
        '\u{2C2B}' => &['\u{2C5B}'], // GLAGOLITIC CAPITAL LETTER IZHITSA
        '\u{2C2C}' => &['\u{2C5C}'], // GLAGOLITIC CAPITAL LETTER SHTAPIC
        '\u{2C2D}' => &['\u{2C5D}'], // GLAGOLITIC CAPITAL LETTER TROKUTASTI A
        '\u{2C2E}' => &['\u{2C5E}'], // GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
        '\u{2C2F}' => &['\u{2C5F}'], // GLAGOLITIC CAPITAL LETTER CAUDATE CHRIVI
        '\u{2C60}' => &['\u{2C61}'], // LATIN CAPITAL LETTER L WITH DOUBLE BAR
        '\u{2C62}' => &['\u{026B}'], // LATIN CAPITAL LETTER L WITH MIDDLE TILDE
        '\u{2C63}' => &['\u{1D7D}'], // LATIN CAPITAL LETTER P WITH STROKE
        '\u{2C64}' => &['\u{027D}'], // LATIN CAPITAL LETTER R WITH TAIL
        '\u{2C67}' => &['\u{2C68}'], // LATIN CAPITAL LETTER H WITH DESCENDER
        '\u{2C69}' => &['\u{2C6A}'], // LATIN CAPITAL LETTER K WITH DESCENDER
        '\u{2C6B}' => &['\u{2C6C}'], // LATIN CAPITAL LETTER Z WITH DESCENDER
        '\u{2C6D}' => &['\u{0251}'], // LATIN CAPITAL LETTER ALPHA
        '\u{2C6E}' => &['\u{0271}'], // LATIN CAPITAL LETTER M WITH HOOK
        '\u{2C6F}' => &['\u{0250}'], // LATIN CAPITAL LETTER TURNED A
        '\u{2C70}' => &['\u{0252}'], // LATIN CAPITAL LETTER TURNED ALPHA
        '\u{2C72}' => &['\u{2C73}'], // LATIN CAPITAL LETTER W WITH HOOK
        '\u{2C75}' => &['\u{2C76}'], // LATIN CAPITAL LETTER HALF H
        '\u{2C7E}' => &['\u{023F}'], // LATIN CAPITAL LETTER S WITH SWASH TAIL
        '\u{2C7F}' => &['\u{0240}'], // LATIN CAPITAL LETTER Z WITH SWASH TAIL
        '\u{2C80}' => &['\u{2C81}'], // COPTIC CAPITAL LETTER ALFA
        '\u{2C82}' => &['\u{2C83}'], // COPTIC CAPITAL LETTER VIDA
        '\u{2C84}' => &['\u{2C85}'], // COPTIC CAPITAL LETTER GAMMA
        '\u{2C86}' => &['\u{2C87}'], // COPTIC CAPITAL LETTER DALDA
        '\u{2C88}' => &['\u{2C89}'], // COPTIC CAPITAL LETTER EIE
        '\u{2C8A}' => &['\u{2C8B}'], // COPTIC CAPITAL LETTER SOU
        '\u{2C8C}' => &['\u{2C8D}'], // COPTIC CAPITAL LETTER ZATA
        '\u{2C8E}' => &['\u{2C8F}'], // COPTIC CAPITAL LETTER HATE
        '\u{2C90}' => &['\u{2C91}'], // COPTIC CAPITAL LETTER THETHE
        '\u{2C92}' => &['\u{2C93}'], // COPTIC CAPITAL LETTER IAUDA
        '\u{2C94}' => &['\u{2C95}'], // COPTIC CAPITAL LETTER KAPA
        '\u{2C96}' => &['\u{2C97}'], // COPTIC CAPITAL LETTER LAULA
        '\u{2C98}' => &['\u{2C99}'], // COPTIC CAPITAL LETTER MI
        '\u{2C9A}' => &['\u{2C9B}'], // COPTIC CAPITAL LETTER NI
        '\u{2C9C}' => &['\u{2C9D}'], // COPTIC CAPITAL LETTER KSI
        '\u{2C9E}' => &['\u{2C9F}'], // COPTIC CAPITAL LETTER O
        '\u{2CA0}' => &['\u{2CA1}'], // COPTIC CAPITAL LETTER PI
        '\u{2CA2}' => &['\u{2CA3}'], // COPTIC CAPITAL LETTER RO
        '\u{2CA4}' => &['\u{2CA5}'], // COPTIC CAPITAL LETTER SIMA
        '\u{2CA6}' => &['\u{2CA7}'], // COPTIC CAPITAL LETTER TAU
        '\u{2CA8}' => &['\u{2CA9}'], // COPTIC CAPITAL LETTER UA
        '\u{2CAA}' => &['\u{2CAB}'], // COPTIC CAPITAL LETTER FI
        '\u{2CAC}' => &['\u{2CAD}'], // COPTIC CAPITAL LETTER KHI
        '\u{2CAE}' => &['\u{2CAF}'], // COPTIC CAPITAL LETTER PSI
        '\u{2CB0}' => &['\u{2CB1}'], // COPTIC CAPITAL LETTER OOU
        '\u{2CB2}' => &['\u{2CB3}'], // COPTIC CAPITAL LETTER DIALECT-P ALEF
        '\u{2CB4}' => &['\u{2CB5}'], // COPTIC CAPITAL LETTER OLD COPTIC AIN
        '\u{2CB6}' => &['\u{2CB7}'], // COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
        '\u{2CB8}' => &['\u{2CB9}'], // COPTIC CAPITAL LETTER DIALECT-P KAPA
        '\u{2CBA}' => &['\u{2CBB}'], // COPTIC CAPITAL LETTER DIALECT-P NI
        '\u{2CBC}' => &['\u{2CBD}'], // COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
        '\u{2CBE}' => &['\u{2CBF}'], // COPTIC CAPITAL LETTER OLD COPTIC OOU
        '\u{2CC0}' => &['\u{2CC1}'], // COPTIC CAPITAL LETTER SAMPI
        '\u{2CC2}' => &['\u{2CC3}'], // COPTIC CAPITAL LETTER CROSSED SHEI
        '\u{2CC4}' => &['\u{2CC5}'], // COPTIC CAPITAL LETTER OLD COPTIC SHEI
        '\u{2CC6}' => &['\u{2CC7}'], // COPTIC CAPITAL LETTER OLD COPTIC ESH
        '\u{2CC8}' => &['\u{2CC9}'], // COPTIC CAPITAL LETTER AKHMIMIC KHEI
        '\u{2CCA}' => &['\u{2CCB}'], // COPTIC CAPITAL LETTER DIALECT-P HORI
        '\u{2CCC}' => &['\u{2CCD}'], // COPTIC CAPITAL LETTER OLD COPTIC HORI
        '\u{2CCE}' => &['\u{2CCF}'], // COPTIC CAPITAL LETTER OLD COPTIC HA
        '\u{2CD0}' => &['\u{2CD1}'], // COPTIC CAPITAL LETTER L-SHAPED HA
        '\u{2CD2}' => &['\u{2CD3}'], // COPTIC CAPITAL LETTER OLD COPTIC HEI
        '\u{2CD4}' => &['\u{2CD5}'], // COPTIC CAPITAL LETTER OLD COPTIC HAT
        '\u{2CD6}' => &['\u{2CD7}'], // COPTIC CAPITAL LETTER OLD COPTIC GANGIA
        '\u{2CD8}' => &['\u{2CD9}'], // COPTIC CAPITAL LETTER OLD COPTIC DJA
        '\u{2CDA}' => &['\u{2CDB}'], // COPTIC CAPITAL LETTER OLD COPTIC SHIMA
        '\u{2CDC}' => &['\u{2CDD}'], // COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
        '\u{2CDE}' => &['\u{2CDF}'], // COPTIC CAPITAL LETTER OLD NUBIAN NGI
        '\u{2CE0}' => &['\u{2CE1}'], // COPTIC CAPITAL LETTER OLD NUBIAN NYI
        '\u{2CE2}' => &['\u{2CE3}'], // COPTIC CAPITAL LETTER OLD NUBIAN WAU
        '\u{2CEB}' => &['\u{2CEC}'], // COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
        '\u{2CED}' => &['\u{2CEE}'], // COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
        '\u{2CF2}' => &['\u{2CF3}'], // COPTIC CAPITAL LETTER BOHAIRIC KHEI
        '\u{A640}' => &['\u{A641}'], // CYRILLIC CAPITAL LETTER ZEMLYA
        '\u{A642}' => &['\u{A643}'], // CYRILLIC CAPITAL LETTER DZELO
        '\u{A644}' => &['\u{A645}'], // CYRILLIC CAPITAL LETTER REVERSED DZE
        '\u{A646}' => &['\u{A647}'], // CYRILLIC CAPITAL LETTER IOTA
        '\u{A648}' => &['\u{A649}'], // CYRILLIC CAPITAL LETTER DJERV
        '\u{A64A}' => &['\u{A64B}'], // CYRILLIC CAPITAL LETTER MONOGRAPH UK
        '\u{A64C}' => &['\u{A64D}'], // CYRILLIC CAPITAL LETTER BROAD OMEGA
        '\u{A64E}' => &['\u{A64F}'], // CYRILLIC CAPITAL LETTER NEUTRAL YER
        '\u{A650}' => &['\u{A651}'], // CYRILLIC CAPITAL LETTER YERU WITH BACK YER
        '\u{A652}' => &['\u{A653}'], // CYRILLIC CAPITAL LETTER IOTIFIED YAT
        '\u{A654}' => &['\u{A655}'], // CYRILLIC CAPITAL LETTER REVERSED YU
        '\u{A656}' => &['\u{A657}'], // CYRILLIC CAPITAL LETTER IOTIFIED A
        '\u{A658}' => &['\u{A659}'], // CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
        '\u{A65A}' => &['\u{A65B}'], // CYRILLIC CAPITAL LETTER BLENDED YUS
        '\u{A65C}' => &['\u{A65D}'], // CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
        '\u{A65E}' => &['\u{A65F}'], // CYRILLIC CAPITAL LETTER YN
        '\u{A660}' => &['\u{A661}'], // CYRILLIC CAPITAL LETTER REVERSED TSE
        '\u{A662}' => &['\u{A663}'], // CYRILLIC CAPITAL LETTER SOFT DE
        '\u{A664}' => &['\u{A665}'], // CYRILLIC CAPITAL LETTER SOFT EL
        '\u{A666}' => &['\u{A667}'], // CYRILLIC CAPITAL LETTER SOFT EM
        '\u{A668}' => &['\u{A669}'], // CYRILLIC CAPITAL LETTER MONOCULAR O
        '\u{A66A}' => &['\u{A66B}'], // CYRILLIC CAPITAL LETTER BINOCULAR O
        '\u{A66C}' => &['\u{A66D}'], // CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
        '\u{A680}' => &['\u{A681}'], // CYRILLIC CAPITAL LETTER DWE
        '\u{A682}' => &['\u{A683}'], // CYRILLIC CAPITAL LETTER DZWE
        '\u{A684}' => &['\u{A685}'], // CYRILLIC CAPITAL LETTER ZHWE
        '\u{A686}' => &['\u{A687}'], // CYRILLIC CAPITAL LETTER CCHE
        '\u{A688}' => &['\u{A689}'], // CYRILLIC CAPITAL LETTER DZZE
        '\u{A68A}' => &['\u{A68B}'], // CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
        '\u{A68C}' => &['\u{A68D}'], // CYRILLIC CAPITAL LETTER TWE
        '\u{A68E}' => &['\u{A68F}'], // CYRILLIC CAPITAL LETTER TSWE
        '\u{A690}' => &['\u{A691}'], // CYRILLIC CAPITAL LETTER TSSE
        '\u{A692}' => &['\u{A693}'], // CYRILLIC CAPITAL LETTER TCHE
        '\u{A694}' => &['\u{A695}'], // CYRILLIC CAPITAL LETTER HWE
        '\u{A696}' => &['\u{A697}'], // CYRILLIC CAPITAL LETTER SHWE
        '\u{A698}' => &['\u{A699}'], // CYRILLIC CAPITAL LETTER DOUBLE O
        '\u{A69A}' => &['\u{A69B}'], // CYRILLIC CAPITAL LETTER CROSSED O
        '\u{A722}' => &['\u{A723}'], // LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
        '\u{A724}' => &['\u{A725}'], // LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
        '\u{A726}' => &['\u{A727}'], // LATIN CAPITAL LETTER HENG
        '\u{A728}' => &['\u{A729}'], // LATIN CAPITAL LETTER TZ
        '\u{A72A}' => &['\u{A72B}'], // LATIN CAPITAL LETTER TRESILLO
        '\u{A72C}' => &['\u{A72D}'], // LATIN CAPITAL LETTER CUATRILLO
        '\u{A72E}' => &['\u{A72F}'], // LATIN CAPITAL LETTER CUATRILLO WITH COMMA
        '\u{A732}' => &['\u{A733}'], // LATIN CAPITAL LETTER AA
        '\u{A734}' => &['\u{A735}'], // LATIN CAPITAL LETTER AO
        '\u{A736}' => &['\u{A737}'], // LATIN CAPITAL LETTER AU
        '\u{A738}' => &['\u{A739}'], // LATIN CAPITAL LETTER AV
        '\u{A73A}' => &['\u{A73B}'], // LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
        '\u{A73C}' => &['\u{A73D}'], // LATIN CAPITAL LETTER AY
        '\u{A73E}' => &['\u{A73F}'], // LATIN CAPITAL LETTER REVERSED C WITH DOT
        '\u{A740}' => &['\u{A741}'], // LATIN CAPITAL LETTER K WITH STROKE
        '\u{A742}' => &['\u{A743}'], // LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
        '\u{A744}' => &['\u{A745}'], // LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
        '\u{A746}' => &['\u{A747}'], // LATIN CAPITAL LETTER BROKEN L
        '\u{A748}' => &['\u{A749}'], // LATIN CAPITAL LETTER L WITH HIGH STROKE
        '\u{A74A}' => &['\u{A74B}'], // LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
        '\u{A74C}' => &['\u{A74D}'], // LATIN CAPITAL LETTER O WITH LOOP
        '\u{A74E}' => &['\u{A74F}'], // LATIN CAPITAL LETTER OO
        '\u{A750}' => &['\u{A751}'], // LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
        '\u{A752}' => &['\u{A753}'], // LATIN CAPITAL LETTER P WITH FLOURISH
        '\u{A754}' => &['\u{A755}'], // LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
        '\u{A756}' => &['\u{A757}'], // LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
        '\u{A758}' => &['\u{A759}'], // LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
        '\u{A75A}' => &['\u{A75B}'], // LATIN CAPITAL LETTER R ROTUNDA
        '\u{A75C}' => &['\u{A75D}'], // LATIN CAPITAL LETTER RUM ROTUNDA
        '\u{A75E}' => &['\u{A75F}'], // LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
        '\u{A760}' => &['\u{A761}'], // LATIN CAPITAL LETTER VY
        '\u{A762}' => &['\u{A763}'], // LATIN CAPITAL LETTER VISIGOTHIC Z
        '\u{A764}' => &['\u{A765}'], // LATIN CAPITAL LETTER THORN WITH STROKE
        '\u{A766}' => &['\u{A767}'], // LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
        '\u{A768}' => &['\u{A769}'], // LATIN CAPITAL LETTER VEND
        '\u{A76A}' => &['\u{A76B}'], // LATIN CAPITAL LETTER ET
        '\u{A76C}' => &['\u{A76D}'], // LATIN CAPITAL LETTER IS
        '\u{A76E}' => &['\u{A76F}'], // LATIN CAPITAL LETTER CON
        '\u{A779}' => &['\u{A77A}'], // LATIN CAPITAL LETTER INSULAR D
        '\u{A77B}' => &['\u{A77C}'], // LATIN CAPITAL LETTER INSULAR F
        '\u{A77D}' => &['\u{1D79}'], // LATIN CAPITAL LETTER INSULAR G
        '\u{A77E}' => &['\u{A77F}'], // LATIN CAPITAL LETTER TURNED INSULAR G
        '\u{A780}' => &['\u{A781}'], // LATIN CAPITAL LETTER TURNED L
        '\u{A782}' => &['\u{A783}'], // LATIN CAPITAL LETTER INSULAR R
        '\u{A784}' => &['\u{A785}'], // LATIN CAPITAL LETTER INSULAR S
        '\u{A786}' => &['\u{A787}'], // LATIN CAPITAL LETTER INSULAR T
        '\u{A78B}' => &['\u{A78C}'], // LATIN CAPITAL LETTER SALTILLO
        '\u{A78D}' => &['\u{0265}'], // LATIN CAPITAL LETTER TURNED H
        '\u{A790}' => &['\u{A791}'], // LATIN CAPITAL LETTER N WITH DESCENDER
        '\u{A792}' => &['\u{A793}'], // LATIN CAPITAL LETTER C WITH BAR
        '\u{A796}' => &['\u{A797}'], // LATIN CAPITAL LETTER B WITH FLOURISH
        '\u{A798}' => &['\u{A799}'], // LATIN CAPITAL LETTER F WITH STROKE
        '\u{A79A}' => &['\u{A79B}'], // LATIN CAPITAL LETTER VOLAPUK AE
        '\u{A79C}' => &['\u{A79D}'], // LATIN CAPITAL LETTER VOLAPUK OE
        '\u{A79E}' => &['\u{A79F}'], // LATIN CAPITAL LETTER VOLAPUK UE
        '\u{A7A0}' => &['\u{A7A1}'], // LATIN CAPITAL LETTER G WITH OBLIQUE STROKE
        '\u{A7A2}' => &['\u{A7A3}'], // LATIN CAPITAL LETTER K WITH OBLIQUE STROKE
        '\u{A7A4}' => &['\u{A7A5}'], // LATIN CAPITAL LETTER N WITH OBLIQUE STROKE
        '\u{A7A6}' => &['\u{A7A7}'], // LATIN CAPITAL LETTER R WITH OBLIQUE STROKE
        '\u{A7A8}' => &['\u{A7A9}'], // LATIN CAPITAL LETTER S WITH OBLIQUE STROKE
        '\u{A7AA}' => &['\u{0266}'], // LATIN CAPITAL LETTER H WITH HOOK
        '\u{A7AB}' => &['\u{025C}'], // LATIN CAPITAL LETTER REVERSED OPEN E
        '\u{A7AC}' => &['\u{0261}'], // LATIN CAPITAL LETTER SCRIPT G
        '\u{A7AD}' => &['\u{026C}'], // LATIN CAPITAL LETTER L WITH BELT
        '\u{A7AE}' => &['\u{026A}'], // LATIN CAPITAL LETTER SMALL CAPITAL I
        '\u{A7B0}' => &['\u{029E}'], // LATIN CAPITAL LETTER TURNED K
        '\u{A7B1}' => &['\u{0287}'], // LATIN CAPITAL LETTER TURNED T
        '\u{A7B2}' => &['\u{029D}'], // LATIN CAPITAL LETTER J WITH CROSSED-TAIL
        '\u{A7B3}' => &['\u{AB53}'], // LATIN CAPITAL LETTER CHI
        '\u{A7B4}' => &['\u{A7B5}'], // LATIN CAPITAL LETTER BETA
        '\u{A7B6}' => &['\u{A7B7}'], // LATIN CAPITAL LETTER OMEGA
        '\u{A7B8}' => &['\u{A7B9}'], // LATIN CAPITAL LETTER U WITH STROKE
        '\u{A7BA}' => &['\u{A7BB}'], // LATIN CAPITAL LETTER GLOTTAL A
        '\u{A7BC}' => &['\u{A7BD}'], // LATIN CAPITAL LETTER GLOTTAL I
        '\u{A7BE}' => &['\u{A7BF}'], // LATIN CAPITAL LETTER GLOTTAL U
        '\u{A7C0}' => &['\u{A7C1}'], // LATIN CAPITAL LETTER OLD POLISH O
        '\u{A7C2}' => &['\u{A7C3}'], // LATIN CAPITAL LETTER ANGLICANA W
        '\u{A7C4}' => &['\u{A794}'], // LATIN CAPITAL LETTER C WITH PALATAL HOOK
        '\u{A7C5}' => &['\u{0282}'], // LATIN CAPITAL LETTER S WITH HOOK
        '\u{A7C6}' => &['\u{1D8E}'], // LATIN CAPITAL LETTER Z WITH PALATAL HOOK
        '\u{A7C7}' => &['\u{A7C8}'], // LATIN CAPITAL LETTER D WITH SHORT STROKE OVERLAY
        '\u{A7C9}' => &['\u{A7CA}'], // LATIN CAPITAL LETTER S WITH SHORT STROKE OVERLAY
        '\u{A7CB}' => &['\u{0264}'], // LATIN CAPITAL LETTER RAMS HORN
        '\u{A7CC}' => &['\u{A7CD}'], // LATIN CAPITAL LETTER S WITH DIAGONAL STROKE
        '\u{A7D0}' => &['\u{A7D1}'], // LATIN CAPITAL LETTER CLOSED INSULAR G
        '\u{A7D6}' => &['\u{A7D7}'], // LATIN CAPITAL LETTER MIDDLE SCOTS S
        '\u{A7D8}' => &['\u{A7D9}'], // LATIN CAPITAL LETTER SIGMOID S
        '\u{A7DA}' => &['\u{A7DB}'], // LATIN CAPITAL LETTER LAMBDA
        '\u{A7DC}' => &['\u{019B}'], // LATIN CAPITAL LETTER LAMBDA WITH STROKE
        '\u{A7F5}' => &['\u{A7F6}'], // LATIN CAPITAL LETTER REVERSED HALF H
        '\u{AB70}' => &['\u{13A0}'], // CHEROKEE SMALL LETTER A
        '\u{AB71}' => &['\u{13A1}'], // CHEROKEE SMALL LETTER E
        '\u{AB72}' => &['\u{13A2}'], // CHEROKEE SMALL LETTER I
        '\u{AB73}' => &['\u{13A3}'], // CHEROKEE SMALL LETTER O
        '\u{AB74}' => &['\u{13A4}'], // CHEROKEE SMALL LETTER U
        '\u{AB75}' => &['\u{13A5}'], // CHEROKEE SMALL LETTER V
        '\u{AB76}' => &['\u{13A6}'], // CHEROKEE SMALL LETTER GA
        '\u{AB77}' => &['\u{13A7}'], // CHEROKEE SMALL LETTER KA
        '\u{AB78}' => &['\u{13A8}'], // CHEROKEE SMALL LETTER GE
        '\u{AB79}' => &['\u{13A9}'], // CHEROKEE SMALL LETTER GI
        '\u{AB7A}' => &['\u{13AA}'], // CHEROKEE SMALL LETTER GO
        '\u{AB7B}' => &['\u{13AB}'], // CHEROKEE SMALL LETTER GU
        '\u{AB7C}' => &['\u{13AC}'], // CHEROKEE SMALL LETTER GV
        '\u{AB7D}' => &['\u{13AD}'], // CHEROKEE SMALL LETTER HA
        '\u{AB7E}' => &['\u{13AE}'], // CHEROKEE SMALL LETTER HE
        '\u{AB7F}' => &['\u{13AF}'], // CHEROKEE SMALL LETTER HI
        '\u{AB80}' => &['\u{13B0}'], // CHEROKEE SMALL LETTER HO
        '\u{AB81}' => &['\u{13B1}'], // CHEROKEE SMALL LETTER HU
        '\u{AB82}' => &['\u{13B2}'], // CHEROKEE SMALL LETTER HV
        '\u{AB83}' => &['\u{13B3}'], // CHEROKEE SMALL LETTER LA
        '\u{AB84}' => &['\u{13B4}'], // CHEROKEE SMALL LETTER LE
        '\u{AB85}' => &['\u{13B5}'], // CHEROKEE SMALL LETTER LI
        '\u{AB86}' => &['\u{13B6}'], // CHEROKEE SMALL LETTER LO
        '\u{AB87}' => &['\u{13B7}'], // CHEROKEE SMALL LETTER LU
        '\u{AB88}' => &['\u{13B8}'], // CHEROKEE SMALL LETTER LV
        '\u{AB89}' => &['\u{13B9}'], // CHEROKEE SMALL LETTER MA
        '\u{AB8A}' => &['\u{13BA}'], // CHEROKEE SMALL LETTER ME
        '\u{AB8B}' => &['\u{13BB}'], // CHEROKEE SMALL LETTER MI
        '\u{AB8C}' => &['\u{13BC}'], // CHEROKEE SMALL LETTER MO
        '\u{AB8D}' => &['\u{13BD}'], // CHEROKEE SMALL LETTER MU
        '\u{AB8E}' => &['\u{13BE}'], // CHEROKEE SMALL LETTER NA
        '\u{AB8F}' => &['\u{13BF}'], // CHEROKEE SMALL LETTER HNA
        '\u{AB90}' => &['\u{13C0}'], // CHEROKEE SMALL LETTER NAH
        '\u{AB91}' => &['\u{13C1}'], // CHEROKEE SMALL LETTER NE
        '\u{AB92}' => &['\u{13C2}'], // CHEROKEE SMALL LETTER NI
        '\u{AB93}' => &['\u{13C3}'], // CHEROKEE SMALL LETTER NO
        '\u{AB94}' => &['\u{13C4}'], // CHEROKEE SMALL LETTER NU
        '\u{AB95}' => &['\u{13C5}'], // CHEROKEE SMALL LETTER NV
        '\u{AB96}' => &['\u{13C6}'], // CHEROKEE SMALL LETTER QUA
        '\u{AB97}' => &['\u{13C7}'], // CHEROKEE SMALL LETTER QUE
        '\u{AB98}' => &['\u{13C8}'], // CHEROKEE SMALL LETTER QUI
        '\u{AB99}' => &['\u{13C9}'], // CHEROKEE SMALL LETTER QUO
        '\u{AB9A}' => &['\u{13CA}'], // CHEROKEE SMALL LETTER QUU
        '\u{AB9B}' => &['\u{13CB}'], // CHEROKEE SMALL LETTER QUV
        '\u{AB9C}' => &['\u{13CC}'], // CHEROKEE SMALL LETTER SA
        '\u{AB9D}' => &['\u{13CD}'], // CHEROKEE SMALL LETTER S
        '\u{AB9E}' => &['\u{13CE}'], // CHEROKEE SMALL LETTER SE
        '\u{AB9F}' => &['\u{13CF}'], // CHEROKEE SMALL LETTER SI
        '\u{ABA0}' => &['\u{13D0}'], // CHEROKEE SMALL LETTER SO
        '\u{ABA1}' => &['\u{13D1}'], // CHEROKEE SMALL LETTER SU
        '\u{ABA2}' => &['\u{13D2}'], // CHEROKEE SMALL LETTER SV
        '\u{ABA3}' => &['\u{13D3}'], // CHEROKEE SMALL LETTER DA
        '\u{ABA4}' => &['\u{13D4}'], // CHEROKEE SMALL LETTER TA
        '\u{ABA5}' => &['\u{13D5}'], // CHEROKEE SMALL LETTER DE
        '\u{ABA6}' => &['\u{13D6}'], // CHEROKEE SMALL LETTER TE
        '\u{ABA7}' => &['\u{13D7}'], // CHEROKEE SMALL LETTER DI
        '\u{ABA8}' => &['\u{13D8}'], // CHEROKEE SMALL LETTER TI
        '\u{ABA9}' => &['\u{13D9}'], // CHEROKEE SMALL LETTER DO
        '\u{ABAA}' => &['\u{13DA}'], // CHEROKEE SMALL LETTER DU
        '\u{ABAB}' => &['\u{13DB}'], // CHEROKEE SMALL LETTER DV
        '\u{ABAC}' => &['\u{13DC}'], // CHEROKEE SMALL LETTER DLA
        '\u{ABAD}' => &['\u{13DD}'], // CHEROKEE SMALL LETTER TLA
        '\u{ABAE}' => &['\u{13DE}'], // CHEROKEE SMALL LETTER TLE
        '\u{ABAF}' => &['\u{13DF}'], // CHEROKEE SMALL LETTER TLI
        '\u{ABB0}' => &['\u{13E0}'], // CHEROKEE SMALL LETTER TLO
        '\u{ABB1}' => &['\u{13E1}'], // CHEROKEE SMALL LETTER TLU
        '\u{ABB2}' => &['\u{13E2}'], // CHEROKEE SMALL LETTER TLV
        '\u{ABB3}' => &['\u{13E3}'], // CHEROKEE SMALL LETTER TSA
        '\u{ABB4}' => &['\u{13E4}'], // CHEROKEE SMALL LETTER TSE
        '\u{ABB5}' => &['\u{13E5}'], // CHEROKEE SMALL LETTER TSI
        '\u{ABB6}' => &['\u{13E6}'], // CHEROKEE SMALL LETTER TSO
        '\u{ABB7}' => &['\u{13E7}'], // CHEROKEE SMALL LETTER TSU
        '\u{ABB8}' => &['\u{13E8}'], // CHEROKEE SMALL LETTER TSV
        '\u{ABB9}' => &['\u{13E9}'], // CHEROKEE SMALL LETTER WA
        '\u{ABBA}' => &['\u{13EA}'], // CHEROKEE SMALL LETTER WE
        '\u{ABBB}' => &['\u{13EB}'], // CHEROKEE SMALL LETTER WI
        '\u{ABBC}' => &['\u{13EC}'], // CHEROKEE SMALL LETTER WO
        '\u{ABBD}' => &['\u{13ED}'], // CHEROKEE SMALL LETTER WU
        '\u{ABBE}' => &['\u{13EE}'], // CHEROKEE SMALL LETTER WV
        '\u{ABBF}' => &['\u{13EF}'], // CHEROKEE SMALL LETTER YA
        '\u{FB00}' => &['\u{0066}', '\u{0066}'], // LATIN SMALL LIGATURE FF
        '\u{FB01}' => &['\u{0066}', '\u{0069}'], // LATIN SMALL LIGATURE FI
        '\u{FB02}' => &['\u{0066}', '\u{006C}'], // LATIN SMALL LIGATURE FL
        '\u{FB03}' => &['\u{0066}', '\u{0066}', '\u{0069}'], // LATIN SMALL LIGATURE FFI
        '\u{FB04}' => &['\u{0066}', '\u{0066}', '\u{006C}'], // LATIN SMALL LIGATURE FFL
        // '\u{FB05}' => &['\u{0073}', '\u{0074}'], // LATIN SMALL LIGATURE LONG S T
        '\u{FB05}' => &['\u{FB06}'], // LATIN SMALL LIGATURE LONG S T
        '\u{FB06}' => &['\u{0073}', '\u{0074}'], // LATIN SMALL LIGATURE ST
        '\u{FB13}' => &['\u{0574}', '\u{0576}'], // ARMENIAN SMALL LIGATURE MEN NOW
        '\u{FB14}' => &['\u{0574}', '\u{0565}'], // ARMENIAN SMALL LIGATURE MEN ECH
        '\u{FB15}' => &['\u{0574}', '\u{056B}'], // ARMENIAN SMALL LIGATURE MEN INI
        '\u{FB16}' => &['\u{057E}', '\u{0576}'], // ARMENIAN SMALL LIGATURE VEW NOW
        '\u{FB17}' => &['\u{0574}', '\u{056D}'], // ARMENIAN SMALL LIGATURE MEN XEH
        '\u{FF21}' => &['\u{FF41}'], // FULLWIDTH LATIN CAPITAL LETTER A
        '\u{FF22}' => &['\u{FF42}'], // FULLWIDTH LATIN CAPITAL LETTER B
        '\u{FF23}' => &['\u{FF43}'], // FULLWIDTH LATIN CAPITAL LETTER C
        '\u{FF24}' => &['\u{FF44}'], // FULLWIDTH LATIN CAPITAL LETTER D
        '\u{FF25}' => &['\u{FF45}'], // FULLWIDTH LATIN CAPITAL LETTER E
        '\u{FF26}' => &['\u{FF46}'], // FULLWIDTH LATIN CAPITAL LETTER F
        '\u{FF27}' => &['\u{FF47}'], // FULLWIDTH LATIN CAPITAL LETTER G
        '\u{FF28}' => &['\u{FF48}'], // FULLWIDTH LATIN CAPITAL LETTER H
        '\u{FF29}' => &['\u{FF49}'], // FULLWIDTH LATIN CAPITAL LETTER I
        '\u{FF2A}' => &['\u{FF4A}'], // FULLWIDTH LATIN CAPITAL LETTER J
        '\u{FF2B}' => &['\u{FF4B}'], // FULLWIDTH LATIN CAPITAL LETTER K
        '\u{FF2C}' => &['\u{FF4C}'], // FULLWIDTH LATIN CAPITAL LETTER L
        '\u{FF2D}' => &['\u{FF4D}'], // FULLWIDTH LATIN CAPITAL LETTER M
        '\u{FF2E}' => &['\u{FF4E}'], // FULLWIDTH LATIN CAPITAL LETTER N
        '\u{FF2F}' => &['\u{FF4F}'], // FULLWIDTH LATIN CAPITAL LETTER O
        '\u{FF30}' => &['\u{FF50}'], // FULLWIDTH LATIN CAPITAL LETTER P
        '\u{FF31}' => &['\u{FF51}'], // FULLWIDTH LATIN CAPITAL LETTER Q
        '\u{FF32}' => &['\u{FF52}'], // FULLWIDTH LATIN CAPITAL LETTER R
        '\u{FF33}' => &['\u{FF53}'], // FULLWIDTH LATIN CAPITAL LETTER S
        '\u{FF34}' => &['\u{FF54}'], // FULLWIDTH LATIN CAPITAL LETTER T
        '\u{FF35}' => &['\u{FF55}'], // FULLWIDTH LATIN CAPITAL LETTER U
        '\u{FF36}' => &['\u{FF56}'], // FULLWIDTH LATIN CAPITAL LETTER V
        '\u{FF37}' => &['\u{FF57}'], // FULLWIDTH LATIN CAPITAL LETTER W
        '\u{FF38}' => &['\u{FF58}'], // FULLWIDTH LATIN CAPITAL LETTER X
        '\u{FF39}' => &['\u{FF59}'], // FULLWIDTH LATIN CAPITAL LETTER Y
        '\u{FF3A}' => &['\u{FF5A}'], // FULLWIDTH LATIN CAPITAL LETTER Z
        '\u{10400}' => &['\u{10428}'], // DESERET CAPITAL LETTER LONG I
        '\u{10401}' => &['\u{10429}'], // DESERET CAPITAL LETTER LONG E
        '\u{10402}' => &['\u{1042A}'], // DESERET CAPITAL LETTER LONG A
        '\u{10403}' => &['\u{1042B}'], // DESERET CAPITAL LETTER LONG AH
        '\u{10404}' => &['\u{1042C}'], // DESERET CAPITAL LETTER LONG O
        '\u{10405}' => &['\u{1042D}'], // DESERET CAPITAL LETTER LONG OO
        '\u{10406}' => &['\u{1042E}'], // DESERET CAPITAL LETTER SHORT I
        '\u{10407}' => &['\u{1042F}'], // DESERET CAPITAL LETTER SHORT E
        '\u{10408}' => &['\u{10430}'], // DESERET CAPITAL LETTER SHORT A
        '\u{10409}' => &['\u{10431}'], // DESERET CAPITAL LETTER SHORT AH
        '\u{1040A}' => &['\u{10432}'], // DESERET CAPITAL LETTER SHORT O
        '\u{1040B}' => &['\u{10433}'], // DESERET CAPITAL LETTER SHORT OO
        '\u{1040C}' => &['\u{10434}'], // DESERET CAPITAL LETTER AY
        '\u{1040D}' => &['\u{10435}'], // DESERET CAPITAL LETTER OW
        '\u{1040E}' => &['\u{10436}'], // DESERET CAPITAL LETTER WU
        '\u{1040F}' => &['\u{10437}'], // DESERET CAPITAL LETTER YEE
        '\u{10410}' => &['\u{10438}'], // DESERET CAPITAL LETTER H
        '\u{10411}' => &['\u{10439}'], // DESERET CAPITAL LETTER PEE
        '\u{10412}' => &['\u{1043A}'], // DESERET CAPITAL LETTER BEE
        '\u{10413}' => &['\u{1043B}'], // DESERET CAPITAL LETTER TEE
        '\u{10414}' => &['\u{1043C}'], // DESERET CAPITAL LETTER DEE
        '\u{10415}' => &['\u{1043D}'], // DESERET CAPITAL LETTER CHEE
        '\u{10416}' => &['\u{1043E}'], // DESERET CAPITAL LETTER JEE
        '\u{10417}' => &['\u{1043F}'], // DESERET CAPITAL LETTER KAY
        '\u{10418}' => &['\u{10440}'], // DESERET CAPITAL LETTER GAY
        '\u{10419}' => &['\u{10441}'], // DESERET CAPITAL LETTER EF
        '\u{1041A}' => &['\u{10442}'], // DESERET CAPITAL LETTER VEE
        '\u{1041B}' => &['\u{10443}'], // DESERET CAPITAL LETTER ETH
        '\u{1041C}' => &['\u{10444}'], // DESERET CAPITAL LETTER THEE
        '\u{1041D}' => &['\u{10445}'], // DESERET CAPITAL LETTER ES
        '\u{1041E}' => &['\u{10446}'], // DESERET CAPITAL LETTER ZEE
        '\u{1041F}' => &['\u{10447}'], // DESERET CAPITAL LETTER ESH
        '\u{10420}' => &['\u{10448}'], // DESERET CAPITAL LETTER ZHEE
        '\u{10421}' => &['\u{10449}'], // DESERET CAPITAL LETTER ER
        '\u{10422}' => &['\u{1044A}'], // DESERET CAPITAL LETTER EL
        '\u{10423}' => &['\u{1044B}'], // DESERET CAPITAL LETTER EM
        '\u{10424}' => &['\u{1044C}'], // DESERET CAPITAL LETTER EN
        '\u{10425}' => &['\u{1044D}'], // DESERET CAPITAL LETTER ENG
        '\u{10426}' => &['\u{1044E}'], // DESERET CAPITAL LETTER OI
        '\u{10427}' => &['\u{1044F}'], // DESERET CAPITAL LETTER EW
        '\u{104B0}' => &['\u{104D8}'], // OSAGE CAPITAL LETTER A
        '\u{104B1}' => &['\u{104D9}'], // OSAGE CAPITAL LETTER AI
        '\u{104B2}' => &['\u{104DA}'], // OSAGE CAPITAL LETTER AIN
        '\u{104B3}' => &['\u{104DB}'], // OSAGE CAPITAL LETTER AH
        '\u{104B4}' => &['\u{104DC}'], // OSAGE CAPITAL LETTER BRA
        '\u{104B5}' => &['\u{104DD}'], // OSAGE CAPITAL LETTER CHA
        '\u{104B6}' => &['\u{104DE}'], // OSAGE CAPITAL LETTER EHCHA
        '\u{104B7}' => &['\u{104DF}'], // OSAGE CAPITAL LETTER E
        '\u{104B8}' => &['\u{104E0}'], // OSAGE CAPITAL LETTER EIN
        '\u{104B9}' => &['\u{104E1}'], // OSAGE CAPITAL LETTER HA
        '\u{104BA}' => &['\u{104E2}'], // OSAGE CAPITAL LETTER HYA
        '\u{104BB}' => &['\u{104E3}'], // OSAGE CAPITAL LETTER I
        '\u{104BC}' => &['\u{104E4}'], // OSAGE CAPITAL LETTER KA
        '\u{104BD}' => &['\u{104E5}'], // OSAGE CAPITAL LETTER EHKA
        '\u{104BE}' => &['\u{104E6}'], // OSAGE CAPITAL LETTER KYA
        '\u{104BF}' => &['\u{104E7}'], // OSAGE CAPITAL LETTER LA
        '\u{104C0}' => &['\u{104E8}'], // OSAGE CAPITAL LETTER MA
        '\u{104C1}' => &['\u{104E9}'], // OSAGE CAPITAL LETTER NA
        '\u{104C2}' => &['\u{104EA}'], // OSAGE CAPITAL LETTER O
        '\u{104C3}' => &['\u{104EB}'], // OSAGE CAPITAL LETTER OIN
        '\u{104C4}' => &['\u{104EC}'], // OSAGE CAPITAL LETTER PA
        '\u{104C5}' => &['\u{104ED}'], // OSAGE CAPITAL LETTER EHPA
        '\u{104C6}' => &['\u{104EE}'], // OSAGE CAPITAL LETTER SA
        '\u{104C7}' => &['\u{104EF}'], // OSAGE CAPITAL LETTER SHA
        '\u{104C8}' => &['\u{104F0}'], // OSAGE CAPITAL LETTER TA
        '\u{104C9}' => &['\u{104F1}'], // OSAGE CAPITAL LETTER EHTA
        '\u{104CA}' => &['\u{104F2}'], // OSAGE CAPITAL LETTER TSA
        '\u{104CB}' => &['\u{104F3}'], // OSAGE CAPITAL LETTER EHTSA
        '\u{104CC}' => &['\u{104F4}'], // OSAGE CAPITAL LETTER TSHA
        '\u{104CD}' => &['\u{104F5}'], // OSAGE CAPITAL LETTER DHA
        '\u{104CE}' => &['\u{104F6}'], // OSAGE CAPITAL LETTER U
        '\u{104CF}' => &['\u{104F7}'], // OSAGE CAPITAL LETTER WA
        '\u{104D0}' => &['\u{104F8}'], // OSAGE CAPITAL LETTER KHA
        '\u{104D1}' => &['\u{104F9}'], // OSAGE CAPITAL LETTER GHA
        '\u{104D2}' => &['\u{104FA}'], // OSAGE CAPITAL LETTER ZA
        '\u{104D3}' => &['\u{104FB}'], // OSAGE CAPITAL LETTER ZHA
        '\u{10570}' => &['\u{10597}'], // VITHKUQI CAPITAL LETTER A
        '\u{10571}' => &['\u{10598}'], // VITHKUQI CAPITAL LETTER BBE
        '\u{10572}' => &['\u{10599}'], // VITHKUQI CAPITAL LETTER BE
        '\u{10573}' => &['\u{1059A}'], // VITHKUQI CAPITAL LETTER CE
        '\u{10574}' => &['\u{1059B}'], // VITHKUQI CAPITAL LETTER CHE
        '\u{10575}' => &['\u{1059C}'], // VITHKUQI CAPITAL LETTER DE
        '\u{10576}' => &['\u{1059D}'], // VITHKUQI CAPITAL LETTER DHE
        '\u{10577}' => &['\u{1059E}'], // VITHKUQI CAPITAL LETTER EI
        '\u{10578}' => &['\u{1059F}'], // VITHKUQI CAPITAL LETTER E
        '\u{10579}' => &['\u{105A0}'], // VITHKUQI CAPITAL LETTER FE
        '\u{1057A}' => &['\u{105A1}'], // VITHKUQI CAPITAL LETTER GA
        '\u{1057C}' => &['\u{105A3}'], // VITHKUQI CAPITAL LETTER HA
        '\u{1057D}' => &['\u{105A4}'], // VITHKUQI CAPITAL LETTER HHA
        '\u{1057E}' => &['\u{105A5}'], // VITHKUQI CAPITAL LETTER I
        '\u{1057F}' => &['\u{105A6}'], // VITHKUQI CAPITAL LETTER IJE
        '\u{10580}' => &['\u{105A7}'], // VITHKUQI CAPITAL LETTER JE
        '\u{10581}' => &['\u{105A8}'], // VITHKUQI CAPITAL LETTER KA
        '\u{10582}' => &['\u{105A9}'], // VITHKUQI CAPITAL LETTER LA
        '\u{10583}' => &['\u{105AA}'], // VITHKUQI CAPITAL LETTER LLA
        '\u{10584}' => &['\u{105AB}'], // VITHKUQI CAPITAL LETTER ME
        '\u{10585}' => &['\u{105AC}'], // VITHKUQI CAPITAL LETTER NE
        '\u{10586}' => &['\u{105AD}'], // VITHKUQI CAPITAL LETTER NJE
        '\u{10587}' => &['\u{105AE}'], // VITHKUQI CAPITAL LETTER O
        '\u{10588}' => &['\u{105AF}'], // VITHKUQI CAPITAL LETTER PE
        '\u{10589}' => &['\u{105B0}'], // VITHKUQI CAPITAL LETTER QA
        '\u{1058A}' => &['\u{105B1}'], // VITHKUQI CAPITAL LETTER RE
        '\u{1058C}' => &['\u{105B3}'], // VITHKUQI CAPITAL LETTER SE
        '\u{1058D}' => &['\u{105B4}'], // VITHKUQI CAPITAL LETTER SHE
        '\u{1058E}' => &['\u{105B5}'], // VITHKUQI CAPITAL LETTER TE
        '\u{1058F}' => &['\u{105B6}'], // VITHKUQI CAPITAL LETTER THE
        '\u{10590}' => &['\u{105B7}'], // VITHKUQI CAPITAL LETTER U
        '\u{10591}' => &['\u{105B8}'], // VITHKUQI CAPITAL LETTER VE
        '\u{10592}' => &['\u{105B9}'], // VITHKUQI CAPITAL LETTER XE
        '\u{10594}' => &['\u{105BB}'], // VITHKUQI CAPITAL LETTER Y
        '\u{10595}' => &['\u{105BC}'], // VITHKUQI CAPITAL LETTER ZE
        '\u{10C80}' => &['\u{10CC0}'], // OLD HUNGARIAN CAPITAL LETTER A
        '\u{10C81}' => &['\u{10CC1}'], // OLD HUNGARIAN CAPITAL LETTER AA
        '\u{10C82}' => &['\u{10CC2}'], // OLD HUNGARIAN CAPITAL LETTER EB
        '\u{10C83}' => &['\u{10CC3}'], // OLD HUNGARIAN CAPITAL LETTER AMB
        '\u{10C84}' => &['\u{10CC4}'], // OLD HUNGARIAN CAPITAL LETTER EC
        '\u{10C85}' => &['\u{10CC5}'], // OLD HUNGARIAN CAPITAL LETTER ENC
        '\u{10C86}' => &['\u{10CC6}'], // OLD HUNGARIAN CAPITAL LETTER ECS
        '\u{10C87}' => &['\u{10CC7}'], // OLD HUNGARIAN CAPITAL LETTER ED
        '\u{10C88}' => &['\u{10CC8}'], // OLD HUNGARIAN CAPITAL LETTER AND
        '\u{10C89}' => &['\u{10CC9}'], // OLD HUNGARIAN CAPITAL LETTER E
        '\u{10C8A}' => &['\u{10CCA}'], // OLD HUNGARIAN CAPITAL LETTER CLOSE E
        '\u{10C8B}' => &['\u{10CCB}'], // OLD HUNGARIAN CAPITAL LETTER EE
        '\u{10C8C}' => &['\u{10CCC}'], // OLD HUNGARIAN CAPITAL LETTER EF
        '\u{10C8D}' => &['\u{10CCD}'], // OLD HUNGARIAN CAPITAL LETTER EG
        '\u{10C8E}' => &['\u{10CCE}'], // OLD HUNGARIAN CAPITAL LETTER EGY
        '\u{10C8F}' => &['\u{10CCF}'], // OLD HUNGARIAN CAPITAL LETTER EH
        '\u{10C90}' => &['\u{10CD0}'], // OLD HUNGARIAN CAPITAL LETTER I
        '\u{10C91}' => &['\u{10CD1}'], // OLD HUNGARIAN CAPITAL LETTER II
        '\u{10C92}' => &['\u{10CD2}'], // OLD HUNGARIAN CAPITAL LETTER EJ
        '\u{10C93}' => &['\u{10CD3}'], // OLD HUNGARIAN CAPITAL LETTER EK
        '\u{10C94}' => &['\u{10CD4}'], // OLD HUNGARIAN CAPITAL LETTER AK
        '\u{10C95}' => &['\u{10CD5}'], // OLD HUNGARIAN CAPITAL LETTER UNK
        '\u{10C96}' => &['\u{10CD6}'], // OLD HUNGARIAN CAPITAL LETTER EL
        '\u{10C97}' => &['\u{10CD7}'], // OLD HUNGARIAN CAPITAL LETTER ELY
        '\u{10C98}' => &['\u{10CD8}'], // OLD HUNGARIAN CAPITAL LETTER EM
        '\u{10C99}' => &['\u{10CD9}'], // OLD HUNGARIAN CAPITAL LETTER EN
        '\u{10C9A}' => &['\u{10CDA}'], // OLD HUNGARIAN CAPITAL LETTER ENY
        '\u{10C9B}' => &['\u{10CDB}'], // OLD HUNGARIAN CAPITAL LETTER O
        '\u{10C9C}' => &['\u{10CDC}'], // OLD HUNGARIAN CAPITAL LETTER OO
        '\u{10C9D}' => &['\u{10CDD}'], // OLD HUNGARIAN CAPITAL LETTER NIKOLSBURG OE
        '\u{10C9E}' => &['\u{10CDE}'], // OLD HUNGARIAN CAPITAL LETTER RUDIMENTA OE
        '\u{10C9F}' => &['\u{10CDF}'], // OLD HUNGARIAN CAPITAL LETTER OEE
        '\u{10CA0}' => &['\u{10CE0}'], // OLD HUNGARIAN CAPITAL LETTER EP
        '\u{10CA1}' => &['\u{10CE1}'], // OLD HUNGARIAN CAPITAL LETTER EMP
        '\u{10CA2}' => &['\u{10CE2}'], // OLD HUNGARIAN CAPITAL LETTER ER
        '\u{10CA3}' => &['\u{10CE3}'], // OLD HUNGARIAN CAPITAL LETTER SHORT ER
        '\u{10CA4}' => &['\u{10CE4}'], // OLD HUNGARIAN CAPITAL LETTER ES
        '\u{10CA5}' => &['\u{10CE5}'], // OLD HUNGARIAN CAPITAL LETTER ESZ
        '\u{10CA6}' => &['\u{10CE6}'], // OLD HUNGARIAN CAPITAL LETTER ET
        '\u{10CA7}' => &['\u{10CE7}'], // OLD HUNGARIAN CAPITAL LETTER ENT
        '\u{10CA8}' => &['\u{10CE8}'], // OLD HUNGARIAN CAPITAL LETTER ETY
        '\u{10CA9}' => &['\u{10CE9}'], // OLD HUNGARIAN CAPITAL LETTER ECH
        '\u{10CAA}' => &['\u{10CEA}'], // OLD HUNGARIAN CAPITAL LETTER U
        '\u{10CAB}' => &['\u{10CEB}'], // OLD HUNGARIAN CAPITAL LETTER UU
        '\u{10CAC}' => &['\u{10CEC}'], // OLD HUNGARIAN CAPITAL LETTER NIKOLSBURG UE
        '\u{10CAD}' => &['\u{10CED}'], // OLD HUNGARIAN CAPITAL LETTER RUDIMENTA UE
        '\u{10CAE}' => &['\u{10CEE}'], // OLD HUNGARIAN CAPITAL LETTER EV
        '\u{10CAF}' => &['\u{10CEF}'], // OLD HUNGARIAN CAPITAL LETTER EZ
        '\u{10CB0}' => &['\u{10CF0}'], // OLD HUNGARIAN CAPITAL LETTER EZS
        '\u{10CB1}' => &['\u{10CF1}'], // OLD HUNGARIAN CAPITAL LETTER ENT-SHAPED SIGN
        '\u{10CB2}' => &['\u{10CF2}'], // OLD HUNGARIAN CAPITAL LETTER US
        '\u{10D50}' => &['\u{10D70}'], // GARAY CAPITAL LETTER A
        '\u{10D51}' => &['\u{10D71}'], // GARAY CAPITAL LETTER CA
        '\u{10D52}' => &['\u{10D72}'], // GARAY CAPITAL LETTER MA
        '\u{10D53}' => &['\u{10D73}'], // GARAY CAPITAL LETTER KA
        '\u{10D54}' => &['\u{10D74}'], // GARAY CAPITAL LETTER BA
        '\u{10D55}' => &['\u{10D75}'], // GARAY CAPITAL LETTER JA
        '\u{10D56}' => &['\u{10D76}'], // GARAY CAPITAL LETTER SA
        '\u{10D57}' => &['\u{10D77}'], // GARAY CAPITAL LETTER WA
        '\u{10D58}' => &['\u{10D78}'], // GARAY CAPITAL LETTER LA
        '\u{10D59}' => &['\u{10D79}'], // GARAY CAPITAL LETTER GA
        '\u{10D5A}' => &['\u{10D7A}'], // GARAY CAPITAL LETTER DA
        '\u{10D5B}' => &['\u{10D7B}'], // GARAY CAPITAL LETTER XA
        '\u{10D5C}' => &['\u{10D7C}'], // GARAY CAPITAL LETTER YA
        '\u{10D5D}' => &['\u{10D7D}'], // GARAY CAPITAL LETTER TA
        '\u{10D5E}' => &['\u{10D7E}'], // GARAY CAPITAL LETTER RA
        '\u{10D5F}' => &['\u{10D7F}'], // GARAY CAPITAL LETTER NYA
        '\u{10D60}' => &['\u{10D80}'], // GARAY CAPITAL LETTER FA
        '\u{10D61}' => &['\u{10D81}'], // GARAY CAPITAL LETTER NA
        '\u{10D62}' => &['\u{10D82}'], // GARAY CAPITAL LETTER PA
        '\u{10D63}' => &['\u{10D83}'], // GARAY CAPITAL LETTER HA
        '\u{10D64}' => &['\u{10D84}'], // GARAY CAPITAL LETTER OLD KA
        '\u{10D65}' => &['\u{10D85}'], // GARAY CAPITAL LETTER OLD NA
        '\u{118A0}' => &['\u{118C0}'], // WARANG CITI CAPITAL LETTER NGAA
        '\u{118A1}' => &['\u{118C1}'], // WARANG CITI CAPITAL LETTER A
        '\u{118A2}' => &['\u{118C2}'], // WARANG CITI CAPITAL LETTER WI
        '\u{118A3}' => &['\u{118C3}'], // WARANG CITI CAPITAL LETTER YU
        '\u{118A4}' => &['\u{118C4}'], // WARANG CITI CAPITAL LETTER YA
        '\u{118A5}' => &['\u{118C5}'], // WARANG CITI CAPITAL LETTER YO
        '\u{118A6}' => &['\u{118C6}'], // WARANG CITI CAPITAL LETTER II
        '\u{118A7}' => &['\u{118C7}'], // WARANG CITI CAPITAL LETTER UU
        '\u{118A8}' => &['\u{118C8}'], // WARANG CITI CAPITAL LETTER E
        '\u{118A9}' => &['\u{118C9}'], // WARANG CITI CAPITAL LETTER O
        '\u{118AA}' => &['\u{118CA}'], // WARANG CITI CAPITAL LETTER ANG
        '\u{118AB}' => &['\u{118CB}'], // WARANG CITI CAPITAL LETTER GA
        '\u{118AC}' => &['\u{118CC}'], // WARANG CITI CAPITAL LETTER KO
        '\u{118AD}' => &['\u{118CD}'], // WARANG CITI CAPITAL LETTER ENY
        '\u{118AE}' => &['\u{118CE}'], // WARANG CITI CAPITAL LETTER YUJ
        '\u{118AF}' => &['\u{118CF}'], // WARANG CITI CAPITAL LETTER UC
        '\u{118B0}' => &['\u{118D0}'], // WARANG CITI CAPITAL LETTER ENN
        '\u{118B1}' => &['\u{118D1}'], // WARANG CITI CAPITAL LETTER ODD
        '\u{118B2}' => &['\u{118D2}'], // WARANG CITI CAPITAL LETTER TTE
        '\u{118B3}' => &['\u{118D3}'], // WARANG CITI CAPITAL LETTER NUNG
        '\u{118B4}' => &['\u{118D4}'], // WARANG CITI CAPITAL LETTER DA
        '\u{118B5}' => &['\u{118D5}'], // WARANG CITI CAPITAL LETTER AT
        '\u{118B6}' => &['\u{118D6}'], // WARANG CITI CAPITAL LETTER AM
        '\u{118B7}' => &['\u{118D7}'], // WARANG CITI CAPITAL LETTER BU
        '\u{118B8}' => &['\u{118D8}'], // WARANG CITI CAPITAL LETTER PU
        '\u{118B9}' => &['\u{118D9}'], // WARANG CITI CAPITAL LETTER HIYO
        '\u{118BA}' => &['\u{118DA}'], // WARANG CITI CAPITAL LETTER HOLO
        '\u{118BB}' => &['\u{118DB}'], // WARANG CITI CAPITAL LETTER HORR
        '\u{118BC}' => &['\u{118DC}'], // WARANG CITI CAPITAL LETTER HAR
        '\u{118BD}' => &['\u{118DD}'], // WARANG CITI CAPITAL LETTER SSUU
        '\u{118BE}' => &['\u{118DE}'], // WARANG CITI CAPITAL LETTER SII
        '\u{118BF}' => &['\u{118DF}'], // WARANG CITI CAPITAL LETTER VIYO
        '\u{16E40}' => &['\u{16E60}'], // MEDEFAIDRIN CAPITAL LETTER M
        '\u{16E41}' => &['\u{16E61}'], // MEDEFAIDRIN CAPITAL LETTER S
        '\u{16E42}' => &['\u{16E62}'], // MEDEFAIDRIN CAPITAL LETTER V
        '\u{16E43}' => &['\u{16E63}'], // MEDEFAIDRIN CAPITAL LETTER W
        '\u{16E44}' => &['\u{16E64}'], // MEDEFAIDRIN CAPITAL LETTER ATIU
        '\u{16E45}' => &['\u{16E65}'], // MEDEFAIDRIN CAPITAL LETTER Z
        '\u{16E46}' => &['\u{16E66}'], // MEDEFAIDRIN CAPITAL LETTER KP
        '\u{16E47}' => &['\u{16E67}'], // MEDEFAIDRIN CAPITAL LETTER P
        '\u{16E48}' => &['\u{16E68}'], // MEDEFAIDRIN CAPITAL LETTER T
        '\u{16E49}' => &['\u{16E69}'], // MEDEFAIDRIN CAPITAL LETTER G
        '\u{16E4A}' => &['\u{16E6A}'], // MEDEFAIDRIN CAPITAL LETTER F
        '\u{16E4B}' => &['\u{16E6B}'], // MEDEFAIDRIN CAPITAL LETTER I
        '\u{16E4C}' => &['\u{16E6C}'], // MEDEFAIDRIN CAPITAL LETTER K
        '\u{16E4D}' => &['\u{16E6D}'], // MEDEFAIDRIN CAPITAL LETTER A
        '\u{16E4E}' => &['\u{16E6E}'], // MEDEFAIDRIN CAPITAL LETTER J
        '\u{16E4F}' => &['\u{16E6F}'], // MEDEFAIDRIN CAPITAL LETTER E
        '\u{16E50}' => &['\u{16E70}'], // MEDEFAIDRIN CAPITAL LETTER B
        '\u{16E51}' => &['\u{16E71}'], // MEDEFAIDRIN CAPITAL LETTER C
        '\u{16E52}' => &['\u{16E72}'], // MEDEFAIDRIN CAPITAL LETTER U
        '\u{16E53}' => &['\u{16E73}'], // MEDEFAIDRIN CAPITAL LETTER YU
        '\u{16E54}' => &['\u{16E74}'], // MEDEFAIDRIN CAPITAL LETTER L
        '\u{16E55}' => &['\u{16E75}'], // MEDEFAIDRIN CAPITAL LETTER Q
        '\u{16E56}' => &['\u{16E76}'], // MEDEFAIDRIN CAPITAL LETTER HP
        '\u{16E57}' => &['\u{16E77}'], // MEDEFAIDRIN CAPITAL LETTER NY
        '\u{16E58}' => &['\u{16E78}'], // MEDEFAIDRIN CAPITAL LETTER X
        '\u{16E59}' => &['\u{16E79}'], // MEDEFAIDRIN CAPITAL LETTER D
        '\u{16E5A}' => &['\u{16E7A}'], // MEDEFAIDRIN CAPITAL LETTER OE
        '\u{16E5B}' => &['\u{16E7B}'], // MEDEFAIDRIN CAPITAL LETTER N
        '\u{16E5C}' => &['\u{16E7C}'], // MEDEFAIDRIN CAPITAL LETTER R
        '\u{16E5D}' => &['\u{16E7D}'], // MEDEFAIDRIN CAPITAL LETTER O
        '\u{16E5E}' => &['\u{16E7E}'], // MEDEFAIDRIN CAPITAL LETTER AI
        '\u{16E5F}' => &['\u{16E7F}'], // MEDEFAIDRIN CAPITAL LETTER Y
        '\u{1E900}' => &['\u{1E922}'], // ADLAM CAPITAL LETTER ALIF
        '\u{1E901}' => &['\u{1E923}'], // ADLAM CAPITAL LETTER DAALI
        '\u{1E902}' => &['\u{1E924}'], // ADLAM CAPITAL LETTER LAAM
        '\u{1E903}' => &['\u{1E925}'], // ADLAM CAPITAL LETTER MIIM
        '\u{1E904}' => &['\u{1E926}'], // ADLAM CAPITAL LETTER BA
        '\u{1E905}' => &['\u{1E927}'], // ADLAM CAPITAL LETTER SINNYIIYHE
        '\u{1E906}' => &['\u{1E928}'], // ADLAM CAPITAL LETTER PE
        '\u{1E907}' => &['\u{1E929}'], // ADLAM CAPITAL LETTER BHE
        '\u{1E908}' => &['\u{1E92A}'], // ADLAM CAPITAL LETTER RA
        '\u{1E909}' => &['\u{1E92B}'], // ADLAM CAPITAL LETTER E
        '\u{1E90A}' => &['\u{1E92C}'], // ADLAM CAPITAL LETTER FA
        '\u{1E90B}' => &['\u{1E92D}'], // ADLAM CAPITAL LETTER I
        '\u{1E90C}' => &['\u{1E92E}'], // ADLAM CAPITAL LETTER O
        '\u{1E90D}' => &['\u{1E92F}'], // ADLAM CAPITAL LETTER DHA
        '\u{1E90E}' => &['\u{1E930}'], // ADLAM CAPITAL LETTER YHE
        '\u{1E90F}' => &['\u{1E931}'], // ADLAM CAPITAL LETTER WAW
        '\u{1E910}' => &['\u{1E932}'], // ADLAM CAPITAL LETTER NUN
        '\u{1E911}' => &['\u{1E933}'], // ADLAM CAPITAL LETTER KAF
        '\u{1E912}' => &['\u{1E934}'], // ADLAM CAPITAL LETTER YA
        '\u{1E913}' => &['\u{1E935}'], // ADLAM CAPITAL LETTER U
        '\u{1E914}' => &['\u{1E936}'], // ADLAM CAPITAL LETTER JIIM
        '\u{1E915}' => &['\u{1E937}'], // ADLAM CAPITAL LETTER CHI
        '\u{1E916}' => &['\u{1E938}'], // ADLAM CAPITAL LETTER HA
        '\u{1E917}' => &['\u{1E939}'], // ADLAM CAPITAL LETTER QAAF
        '\u{1E918}' => &['\u{1E93A}'], // ADLAM CAPITAL LETTER GA
        '\u{1E919}' => &['\u{1E93B}'], // ADLAM CAPITAL LETTER NYA
        '\u{1E91A}' => &['\u{1E93C}'], // ADLAM CAPITAL LETTER TU
        '\u{1E91B}' => &['\u{1E93D}'], // ADLAM CAPITAL LETTER NHA
        '\u{1E91C}' => &['\u{1E93E}'], // ADLAM CAPITAL LETTER VA
        '\u{1E91D}' => &['\u{1E93F}'], // ADLAM CAPITAL LETTER KHA
        '\u{1E91E}' => &['\u{1E940}'], // ADLAM CAPITAL LETTER GBE
        '\u{1E91F}' => &['\u{1E941}'], // ADLAM CAPITAL LETTER ZAL
        '\u{1E920}' => &['\u{1E942}'], // ADLAM CAPITAL LETTER KPO
        '\u{1E921}' => &['\u{1E943}'], // ADLAM CAPITAL LETTER SHA
        _ => slice::from_ref(ch),
    })
}

fn char_switch_case<I: Iterator<Item = char> + ExactSizeIterator>(
    ch: char,
    operation: fn(char) -> I,
) -> Result<char, Exception> {
    let mut ch = operation(ch);
    let len = ch.len();
    if len == 1 {
        Ok(ch.next().unwrap())
    } else {
        Err(Exception::wrong_num_of_unicode_chars(1, len))
    }
}
fn char_switch_case_ref<'a, I: Iterator<Item = char> + ExactSizeIterator + 'a>(
    ch: &'a char,
    operation: fn(&'a char) -> I,
) -> Result<char, Exception> {
    let mut ch = operation(ch);
    let len = ch.len();
    if len == 1 {
        Ok(ch.next().unwrap())
    } else {
        Err(Exception::wrong_num_of_unicode_chars(1, len))
    }
}

#[bridge(name = "char->integer", lib = "(base)")]
pub async fn char_to_integer(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let ch = ch.read();
    let ch: char = ch.as_ref().try_into()?;

    Ok(vec![Gc::new(Value::Number(Number::FixedInteger(
        <char as Into<u32>>::into(ch).into(),
    )))])
}

#[bridge(name = "integer->char", lib = "(base)")]
pub async fn integer_to_char(int: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let int = int.read();
    let int: &Number = int.as_ref().try_into()?;
    let int: u64 = int.to_u64();
    if let Ok(int) = <u64 as TryInto<u32>>::try_into(int) {
        if let Some(ch) = char::from_u32(int) {
            return Ok(vec![Gc::new(Value::Character(ch))]);
        }
    }

    // char->integer returns a number larger than 0x10FFFF if integer is not an unicode scalar
    Ok(vec![Gc::new(Value::Number(Number::FixedInteger(
        0x10FFFF + 1,
    )))])
}

macro_rules! impl_char_operator {
    (
        $(($bridge_name:literal,
        $function_name:ident,
        $cmp_function:ident)),* $(,)?
    ) => {
        $(#[bridge(name = $bridge_name, lib = "(base)")]
        pub async fn $function_name(req_lhs: &Gc<Value>, req_rhs: &Gc<Value>, opt_chars: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
            for window in [req_lhs, req_rhs]
                .into_iter()
                .chain(opt_chars)
                .map(|ch| {
                    let ch = ch.read();
                    ch.as_ref().try_into()
                })
                .collect::<Result<Vec<char>, Exception>>()?
                .windows(2) {

                if !window.first()
                    .and_then(|lhs| Some((lhs, window.get(1)?)))
                    .map(|(lhs, rhs)| lhs.$cmp_function(rhs))
                    .unwrap_or(true) {
                    return Ok(vec![Gc::new(Value::Boolean(false))]);
                }
            }

            Ok(vec![Gc::new(Value::Boolean(true))])
        })*
    }
}
impl_char_operator![
    ("char=?", char_eq, eq),
    ("char<?", char_lt, lt),
    ("char>?", char_gt, gt),
    ("char>=?", char_ge, ge),
    ("char<=?", char_le, le),
];

macro_rules! impl_char_ci_operator {
    (
        $(($bridge_name:literal,
        $function_name:ident,
        $cmp_function:ident)),* $(,)?
    ) => {
        $(#[bridge(name = $bridge_name, lib = "(base)")]
        pub async fn $function_name(req_lhs: &Gc<Value>, req_rhs: &Gc<Value>, opt_chars: &[Gc<Value>]) -> Result<Vec<Gc<Value>>, Exception> {
            for window in [req_lhs, req_rhs]
                .into_iter()
                .chain(opt_chars)
                .map(|ch| {
                    let ch = ch.read();
                    <&Value as TryInto<char>>::try_into(ch.as_ref())
                        .and_then(|c| char_switch_case_ref(&c, to_foldcase))
                })
                .collect::<Result<Vec<char>, Exception>>()?
                .windows(2) {

                if !window.first()
                    .and_then(|lhs| Some((lhs, window.get(1)?)))
                    .map(|(lhs, rhs)| lhs.$cmp_function(rhs))
                    .unwrap_or(true) {
                    return Ok(vec![Gc::new(Value::Boolean(false))]);
                }
            }

            Ok(vec![Gc::new(Value::Boolean(true))])
        })*
    }
}
impl_char_ci_operator![
    ("char-ci-=?", char_ci_eq, eq),
    ("char-ci-<?", char_ci_lt, lt),
    ("char-ci->?", char_ci_gt, gt),
    ("char-ci->=?", char_ci_ge, ge),
    ("char-ci-<=?", char_ci_le, le),
];

macro_rules! impl_char_predicate {
    ($(($bridge_name:literal, $function_name:ident, $predicate:ident)),* $(,)?) => {
        $(#[bridge(name = $bridge_name, lib = "(base)")]
        pub async fn $function_name(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
            let ch = ch.read();
            let ch: char = ch.as_ref().try_into()?;
            Ok(vec![Gc::new(Value::Boolean(ch.$predicate()))])
        })*
    }
}
impl_char_predicate![
    ("char-alphabetic?", char_is_alphabetic, is_ascii_alphabetic),
    ("char-numeric?", char_is_numeric, is_number_decimal_digit),
    ("char-whitespace?", char_is_whitespace, is_whitespace),
    ("char-upper?", char_is_uppercase, is_uppercase),
    ("char-lower?", char_is_lowercase, is_lowercase),
];

#[bridge(name = "digit-value", lib = "(base)")]
pub async fn digit_value(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let ch = ch.read();
    let ch: char = ch.as_ref().try_into()?;

    Ok(vec![Gc::new(
        digit_to_num(ch)
            .map(Number::FixedInteger)
            .map(Value::Number)
            .unwrap_or(Value::Boolean(false)),
    )])
}

macro_rules! impl_char_case_converter {
    ($(($bridge_name:literal, $function_name:ident, $converter:expr)),* $(,)?) => {
        $(#[bridge(name = $bridge_name, lib = "(base)")]
        pub async fn $function_name(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
            let ch = ch.read();
            let ch: char = ch.as_ref().try_into()?;
            Ok(vec![Gc::new(Value::Character(char_switch_case(ch, $converter)?))])
        })*
    }
}
impl_char_case_converter![
    ("char-upcase", char_upcase, char::to_uppercase),
    ("char-downcase", char_downcase, char::to_lowercase),
];

#[bridge(name = "char-foldcase", lib = "(base)")]
pub async fn char_foldcase(ch: &Gc<Value>) -> Result<Vec<Gc<Value>>, Exception> {
    let ch = ch.read();
    let ch: &char = ch.as_ref().try_into()?;
    Ok(vec![Gc::new(Value::Character(char_switch_case_ref(
        ch,
        to_foldcase,
    )?))])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digit_to_num() {
        (char::MIN..char::MAX)
            .filter(|c| c.is_number_decimal_digit())
            .map(digit_to_num)
            .for_each(|d| assert!(d.is_some()));
    }
}
