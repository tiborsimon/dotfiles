#!/usr/bin/env python
import fileinput
import pydoc
import re
import sys
import unittest


# TODO: Implement proper lexer + parser.
# TODO: Add header date corrector, if a header date and weekday is off.

# ============================================================================
#  C O N F I G U R A T I O N

INDENT_SPACES = 4
LINE_WIDTH = 80
POSTING_COMMENT_INDENT_SPACES = 2

# ============================================================================
#  D O C U M E N T A T I O N

VERSION = 'v1.1.2'

HELP_STRING = """\

  {bold}LEDGER-FORMATTER {yellow}{version}{reset}

    {bold}my-ledger-formatter {green}[ledger file to format]{reset}

  File formatter for ledger files. It uses predefined formatting definition
  controlled by global variables in the script itself.

    {bold}{green}[file path]{reset}     - File to format, \
if missing stdin will be used.
    {bold}{blue}(-h|--help){reset}     - Prints out this help.
    {bold}{blue}(-v|--version){reset}  - Prints out the version.

  {bold}FEATURES{reset}

  Postings will be formatted in the following way:

    |{blue}<---------------{width}-------------->{reset}|
    |{bold}{yellow}## =============..==============={reset}|
    |{bold}{yellow}## 2019-01-01 - Tuesday{reset}|
    |{blue}<-{indent}->{blue}{bold}{yellow}MyAccount{reset}\
{blue}<-..->{reset}{bold}{red}[= ]{reset}\
{bold}{yellow}1,234 EUR{reset}|
    |{blue}<-{indent}->{blue}{bold}{yellow}MyAccount{reset}\
{blue}<-..->{reset}{bold}{red}[= ]{reset}\
{bold}{yellow}1,234 EUR{reset}\
{blue}<-{comment}->{reset}{bold}{yellow}; comment{reset}|
    |{blue}<-{indent}->{blue}{bold}{yellow}MyAccount{reset}|
    |{blue}<-{indent}->{blue}{bold}{yellow}; :tag1:tag2:{reset}|

  Invalid lines will be left as is. The files will be formatted in place. If
  the operation is interrupted, the file will be corrupted. It is heavily
  suggested that you use a version control system to prevent data loss due to
  formatting.

  {bold}CHANGELOG{reset}

  {bold}{yellow}v1.1.2{reset} - {bold}2019-01-05{reset}
  Negative numbers can be indented.

  {bold}{yellow}v1.1.1{reset} - {bold}2019-01-05{reset}
  Number formatting bug fixed.

  {bold}{yellow}v1.1.0{reset} - {bold}2019-01-05{reset}
  Standard input handling added.

  {bold}{yellow}v1.0.0{reset} - {bold}2019-01-05{reset}
  First implementation. Be able to handle tags, closing posting, postings
  with balancing equal sign and comments. Test suite included.
""".format(bold='\033[1m',
           blue='\033[94m',
           green='\033[92m',
           yellow='\033[93m',
           red='\033[91m',
           reset='\033[0m',
           version=VERSION,
           indent=INDENT_SPACES,
           width=LINE_WIDTH,
           comment=POSTING_COMMENT_INDENT_SPACES)


# ============================================================================
#  B U S I N E S S   L O G I C

INDENT = ' '*INDENT_SPACES
WIDTH = LINE_WIDTH
POSTING_COMMENT_INDENT = ' '*POSTING_COMMENT_INDENT_SPACES


def _only_tags(tokens):
    return tokens[0].startswith(';')


def _parse_cost(tokens):
    parsed = []
    number_was_found = False
    for token in tokens:
        if re.match(r'^[\d\.,-]+$', token):
            sanitized = token.replace(',', '')
            if '.' in sanitized:
                formatted = '{:,}'.format(float(sanitized))
            else:
                formatted = '{:,}'.format(int(sanitized))
            parsed.append(formatted)
            number_was_found = True
        else:
            parsed.append(token)
    if not number_was_found:
        raise SyntaxError('No number in the posting!')
    return ' '.join(parsed)


def _parse_comment(tokens):
    comment = ' '.join(tokens)
    comment = comment.strip()
    comment = comment.split(';')
    comment = '; ' + ' '.join(comment).strip()
    return POSTING_COMMENT_INDENT + comment


def _separate_cost_and_comments(tokens):
    cost = []
    comment = []
    for token in tokens:
        if not comment:
            if token.startswith(';'):
                comment.append(token)
            else:
                cost.append(token)
        else:
            comment.append(token)
    return (cost, comment)


def _posting_has_comment(tokens):
    for token in tokens:
        if token.startswith(';'):
            return True
    return False


def process(line):
    # Day starter header
    if line.startswith('## =='):
        h1 = '## '
        return h1 + '='*(WIDTH - len(h1))

    # Postings indentation
    # Valid formats:
    if line.startswith('  '):
        tokens = line.strip().split()

        if _only_tags(tokens):
            # tags branch
            # example: "    ; :tag1:tag2:"
            return INDENT + ' '.join(tokens)
        else:
            # postings branch
            account = tokens.pop(0)

            # check if there are anything left after the account
            if tokens:
                # posting with cost and others
                # example: "    Account:Something     100 HUF"
                # example: "    Account:Something     = 100 HUF"
                # example: "    Account:Something     100 HUF ; hello"
                if _posting_has_comment(tokens):
                    cost, comment = _separate_cost_and_comments(tokens)

                    cost = _parse_cost(cost)
                    comment = _parse_comment(comment)

                    padding = WIDTH - len(INDENT + account + cost)
                    line = (INDENT + account + ' '*padding + cost + comment)
                else:
                    # posting without a comment
                    cost = _parse_cost(tokens)
                    padding = WIDTH - len(INDENT) - len(account) - len(cost)
                    line = INDENT + account + ' '*padding + cost
            else:
                # closing posting
                # example: "    Account:Something"
                line = INDENT + account

            return line.rstrip()
    return line


def main(target=None):
    if target:
        source = fileinput.input([target], inplace=True)
    else:
        source = fileinput.input()

    for line in source:
        line = line.rstrip()
        try:
            line = process(line)
        except Exception:
            # leave the line as is on error..
            pass
        sys.stdout.write(line + '\n')


# ============================================================================
#  U N I T T E S T   S U I T E

class HeaderLineCases(unittest.TestCase):
    def test__header_separator_can_be_extended_to_length(self):
        h1 = '## '
        line = h1 + '====='
        expected = h1 + '='*(WIDTH - len(h1))
        result = process(line)
        self.assertEqual(expected, result)

    def test__header_separator_can_be_trimmed_to_length(self):
        h1 = '## '
        line = h1 + '='*200
        expected = h1 + '='*(WIDTH - len(h1))
        result = process(line)
        self.assertEqual(expected, result)


class TagCases(unittest.TestCase):
    def test__tags_can_be_parsed_and_indented(self):
        t1 = '; :tag:'
        line = '   ' + t1 + ' '
        expected = INDENT + t1
        result = process(line)
        self.assertEqual(expected, result)

    def test__multiple_tags_can_be_parsed_and_indented(self):
        t1 = '; :tag1:tag2:'
        line = '   ' + t1 + '   '
        expected = INDENT + t1
        result = process(line)
        self.assertEqual(expected, result)


class ClosingPostingCases(unittest.TestCase):
    def test__end_postings_not_modified(self):
        line = INDENT + 'Assets:Something'
        expected = INDENT + 'Assets:Something'
        result = process(line)
        self.assertEqual(expected, result)

    def test__end_postings_trailing_whitespace_trimmed(self):
        line = INDENT + 'Assets:Something' + '       '
        expected = INDENT + 'Assets:Something'
        result = process(line)
        self.assertEqual(expected, result)


class PostingCases(unittest.TestCase):
    def test__posting_values_aligned_right(self):
        p1 = 'Assets:Something'
        p2 = '100 EUR'
        line = INDENT + p1 + '      ' + p2
        padding = (WIDTH - len(INDENT) - len(p1) - len(p2))
        expected = INDENT + p1 + ' '*padding + p2
        result = process(line)
        self.assertEqual(expected, result)

    def test__posting_with_complex_cost(self):
        p1 = 'Assets:Something'
        p2 = '= 1,000 EUR'
        line = INDENT + p1 + '      ' + p2
        padding = (WIDTH - len(INDENT) - len(p1) - len(p2))
        expected = INDENT + p1 + ' '*padding + p2
        result = process(line)
        self.assertEqual(expected, result)


class PostingCommentCases(unittest.TestCase):
    def test__comments_can_be_handled(self):
        p1 = 'Assets:Something'
        p2 = '100 EUR'
        comment = '; comment'  # valid comment format
        line = INDENT + p1 + '      ' + p2 + ' ' + comment

        padding = (WIDTH - len(INDENT) - len(p1) - len(p2))
        expected = (INDENT + p1 + ' '*padding + p2
                    + POSTING_COMMENT_INDENT + comment)

        result = process(line)
        self.assertEqual(expected, result)

    def test__comments_with_no_whitespace_can_be_handled(self):
        p1 = 'Assets:Something'
        p2 = '100 EUR'
        comment = ';comment'  # comment with no space before
        line = INDENT + p1 + '      ' + p2 + ' ' + comment

        padding = (WIDTH - len(INDENT) - len(p1) - len(p2))
        expected = (INDENT + p1 + ' '*padding + p2
                    + POSTING_COMMENT_INDENT + '; comment')

        result = process(line)
        self.assertEqual(expected, result)

    def test__comments_with_more_whitespace_can_be_handled(self):
        p1 = 'Assets:Something'
        p2 = '100 EUR'
        comment = ';        comment    comment'  # comment with no space before
        line = INDENT + p1 + '      ' + p2 + ' ' + comment

        padding = (WIDTH - len(INDENT) - len(p1) - len(p2))
        expected = (INDENT + p1 + ' '*padding + p2
                    + POSTING_COMMENT_INDENT + '; comment comment')

        result = process(line)
        self.assertEqual(expected, result)


class CostParsingCases(unittest.TestCase):
    def test__simple_cost_can_be_parsed(self):
        cost = '100 EUR'.split()
        expected = '100 EUR'
        result = _parse_cost(cost)
        self.assertEqual(expected, result)

    def test__equal_sign_can_be_handled(self):
        cost = '= 100 EUR'.split()
        expected = '= 100 EUR'
        result = _parse_cost(cost)
        self.assertEqual(expected, result)

    def test__separator_and_decimal_point_can_be_handled(self):
        cost = '1,000.01 EUR'.split()
        expected = '1,000.01 EUR'
        result = _parse_cost(cost)
        self.assertEqual(expected, result)

    def test__separator_can_be_added_if_missing(self):
        cost = '1000 EUR'.split()
        expected = '1,000 EUR'
        result = _parse_cost(cost)
        self.assertEqual(expected, result)

    def test__separator_can_be_added_if_missing_with_decimals(self):
        cost = '1000000.5 EUR'.split()
        expected = '1,000,000.5 EUR'
        result = _parse_cost(cost)
        self.assertEqual(expected, result)

    def test__separator_can_be_added_if_missing_with_small_decimals(self):
        cost = '1000000.00005 EUR'.split()
        expected = '1,000,000.00005 EUR'
        result = _parse_cost(cost)
        self.assertEqual(expected, result)


class FeatureValidUseCases(unittest.TestCase):
    def test__closing_posting(self):
        p1 = 'Assets:Something'
        line = '  ' + p1 + '              '
        expected = INDENT + p1
        result = process(line)
        self.assertEqual(expected, result)

    def test__posting_with_value_only_no_decimal_point(self):
        p1 = 'Assets:Something'
        p2 = '40000 '
        p3 = 'EUR'
        line = '  ' + p1 + '      ' + p2 + '   ' + p3
        p2 = '40,000 '
        padding = (WIDTH - len(INDENT+p1+p2+p3))
        expected = INDENT + p1 + ' '*padding + p2 + p3
        result = process(line)
        self.assertEqual(expected, result)

    def test__posting_with_negaive_value(self):
        p1 = 'Assets:Something'
        p2 = '-40000 '
        p3 = 'EUR'
        line = '  ' + p1 + '      ' + p2 + '   ' + p3
        p2 = '-40,000 '
        padding = (WIDTH - len(INDENT+p1+p2+p3))
        expected = INDENT + p1 + ' '*padding + p2 + p3
        result = process(line)
        self.assertEqual(expected, result)

    def test__posting_with_value_only_decimal_point(self):
        p1 = 'Assets:Something'
        p2 = '40000.56 '
        p3 = 'EUR'
        line = '  ' + p1 + '      ' + p2 + '   ' + p3
        p2 = '40,000.56 '
        padding = (WIDTH - len(INDENT+p1+p2+p3))
        expected = INDENT + p1 + ' '*padding + p2 + p3
        result = process(line)
        self.assertEqual(expected, result)

    def test__posting_with_comment(self):
        p1 = 'Assets:Something'
        p2 = '40000.56 '
        p3 = 'EUR'
        p4 = ';comment'
        line = ('  ' + p1 + '      ' + p2 + '   ' + p3
                + '    ' + p4)
        p2 = '40,000.56 '
        p4 = '; comment'
        padding = (WIDTH - len(INDENT+p1+p2+p3))
        expected = (INDENT + p1 + ' '*padding + p2 + p3
                    + POSTING_COMMENT_INDENT + p4)
        result = process(line)
        self.assertEqual(expected, result)

    def test__posting_with_ballance_adjust_and_comment(self):
        p1 = 'Assets:Something'
        p2 = '= '
        p3 = '40000.56 '
        p4 = 'EUR'
        p5 = ';comment'
        line = ('  ' + p1 + '      ' + p2 + '   ' + p3
                + '    ' + p4 + '     ' + p5)
        p3 = '40,000.56 '
        p5 = '; comment'
        padding = (WIDTH - len(INDENT+p1+p2+p3+p4))
        expected = (INDENT + p1 + ' '*padding + p2 + p3 + p4
                    + POSTING_COMMENT_INDENT + p5)
        result = process(line)
        self.assertEqual(expected, result)

    def test__one_tag(self):
        t1 = '; :tag1:'
        line = '  ' + t1 + '          '
        expected = INDENT + t1
        result = process(line)
        self.assertEqual(expected, result)

    def test__two_tags(self):
        t1 = '; :tag1:tag2:'
        line = '  ' + t1 + '        '
        t1 = '; :tag1:tag2:'
        expected = INDENT + t1
        result = process(line)
        self.assertEqual(expected, result)


class FeatureInvalidUseCases(unittest.TestCase):
    def test__posting_with_no_numeric_value__exception_should_raised(self):
        p1 = 'Assets:Something'
        p2 = 'hello '
        p3 = 'EUR'
        line = '  ' + p1 + '      ' + p2 + '   ' + p3
        with self.assertRaises(SyntaxError):
            process(line)


# ============================================================================
#  S C R I P T   E N T R Y   P O I N T

if __name__ == '__main__':

    if len(sys.argv) > 1:
        if sys.argv[1] in ('-t', '--test'):
            # unit testing entry point
            # A hack to normalize the arguments for the unittests..
            sys.argv = sys.argv[:1]
            unittest.main()

        elif sys.argv[1] in ('-h', '--help'):
            # explicit help printout
            pydoc.pipepager(HELP_STRING, cmd='less -R')

        elif sys.argv[1] in ('-v', '--version'):
            # version printout
            print(VERSION)

        elif len(sys.argv) == 2:
            # file specified, read and modify it
            target = sys.argv[1]
            main(target)

        else:
            # implicit help menu on invalid arguments
            pydoc.pipepager(HELP_STRING, cmd='less -R')

    elif len(sys.argv) == 1:
        # using standard input as source
        main()

    else:
        # implicit help menu on invalid arguments
        pydoc.pipepager(HELP_STRING, cmd='less -R')
