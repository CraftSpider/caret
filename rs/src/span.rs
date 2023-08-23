
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct Span<'a> {
    file: &'a str,
    range: Range<usize>,
}

impl<'a> chumsky::Span for Span<'a> {
    type Context = &'a str;
    type Offset = usize;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Span {
            file: context,
            range,
        }
    }

    fn context(&self) -> Self::Context {
        self.file
    }

    fn start(&self) -> Self::Offset {
        self.range.start
    }

    fn end(&self) -> Self::Offset {
        self.range.end
    }
}
