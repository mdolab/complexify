#!/usr/bin/env python

import sys
import os
import glob
import re
import argparse



def main():

    parser = argparse.ArgumentParser(description="Complexify one or more fortran files. By default the complexified file is written to the same directory as the input files with a 'c_' prefix.")
    parser.add_argument("filePattern", nargs="+", help="One or more files that should be complexified.")
    parser.add_argument("--fix_relationals", type=str, choices=["lucky_logic", "MIPS_logic"], help="lucky_logic: don't attempt to fix .eq. and .ne. (works on PGF90), MIPS_logic: bug in MIPS pro V7.3 reqiures .ge. fixed too")
    parser.add_argument("--fudge_format", action="store_true", help="don't attempt to fix .eq. and .ne. (works on PGF90)")
    parser.add_argument("-o", "--outFileName", type=str, default=None, help="The complexified filename (and path if desired). Note: This is only allowed for a single input file.")
    args = parser.parse_args()

    fix_relationals = 1
    if args.fix_relationals == "lucky_logic":
        # don't attempt to fix .eq. and .ne. (works on PGF90)
        fix_relationals = 0
    elif args.fix_relationals == "MIPS_logic":
        # cheap fix for MIPS Pro
        fix_relationals = 2

    if len(args.filePattern) > 1 and args.outFileName is not None:
        print(f"outFileName cannot be used with more than one input argument, {len(args.filePattern)} given. Check input.")
        sys.exit(1)

    nFailedFiles = 0
    for fileName in args.filePattern:

        if os.path.islink(fileName):
            # Print a to stderr
            print(f"{fileName} : will not process symbolic links", file=sys.stderr)
            nFailedFiles = 1
        else:
            # Pattern could include a wildcard, so process
            for file in glob.glob(fileName):
                nFailedFiles += fix_file(file, fix_relationals, args.fudge_format, args.outFileName)

    status = 0
    if nFailedFiles > 0:
        print(f"Conversion of {nFailedFiles} files failed. Check output.")
        status = 1

    sys.exit(status)


# Compile all regular expressions
patt_real = re.compile(r"^\s*real\b", re.IGNORECASE)
patt_always_real = re.compile("^\s*real\(kind=alwaysRealType", re.IGNORECASE)
patt_cgns_real = re.compile("^\s*real\(kind=cgnsRealType", re.IGNORECASE)

patt_real_s = re.compile(r"real\b(?:\s*\*\s*)?([48])?", re.IGNORECASE)
patt_double_s = re.compile(r"double\s*precision", re.IGNORECASE)
patt_double = re.compile("(^\s*)double\s*precision(\s+.*)", re.IGNORECASE)
patt_mpi_stuff = re.compile(r"mpi_double_precision|mpi_real8|mpi_real4|mpi_real", re.IGNORECASE)
patt_implicit = re.compile(r"^\s*implicit\b", re.IGNORECASE)
patt_comment = re.compile("^\s*!.*", re.IGNORECASE)
patt_inc = re.compile(r'(\s*\d*\s*)include\s*("|\')(\w+.?\w*)(?:"|\')(.*)', re.IGNORECASE)

patt_ge = re.compile(r"(?:>=)|(?:\.\s*ge\s*\.)", re.IGNORECASE)
patt_eq = re.compile(r"(?:==)|(?:\.\s*eq\s*\.)", re.IGNORECASE)
patt_ne = re.compile(r"(?:/=)|(?:\.\s*ne\s*\.)", re.IGNORECASE)
# patt_if = re.compile('(\s*\d*(?:\s*)|(?:\s*else\s*)|(\s*\w+\:\s*))(if\s*\()((?:.|\n)*$)',re.IGNORECASE|re.DOTALL)
patt_if = re.compile("((?:\s*)|(?:\s*\w+:\s*))((?:else\s*|)if\s*\()(.*$)", re.IGNORECASE | re.DOTALL)
# patt_if = re.compile('(\s*\d*(?:\s*)|(?:\s*else\s*))(if\s*\()((?:.|\n)*$)',
#                     re.IGNORECASE)  # includes '\n' at end  # ORIGINAL
# patt_if = re.compile('(.*if.*)',
#                    re.IGNORECASE)  # includes '\n' at end
patt_logic_assignment = re.compile(
    "([^=]*)(\s*=[ \t]*)((?:.|\n)*\.\s*(?:and|or|not|eqv|neqv)\s*\.(?:.|\n)*)", re.IGNORECASE
)

patt_subroutine = re.compile(r"\s*(?!end)\w*\s*subroutine\b", re.IGNORECASE)
patt_program = re.compile(r"^\s*program\b", re.IGNORECASE)
patt_function = re.compile(r"^\s*(?!end)\w*\s*function\b\s*\w+\s*\(", re.IGNORECASE)
patt_module_procedure = re.compile(r"^\s*module procedure", re.IGNORECASE)
patt_real_cast = re.compile(r"([^a-zA-z]\s*)real\s*\(", re.IGNORECASE)
patt_module = re.compile(r"^\s*(?!end)\s*module\b", re.IGNORECASE)

patt_use = re.compile(r"^\s*use\s*\w+", re.IGNORECASE)
patt_intrinsic = re.compile(
    r"^\s*(?:(?:complex|real|integer|logical|character).*,.*)?intrinsic\b(?:\s*:\s*:)?", re.IGNORECASE
)
patt_format = re.compile(r"^\s*[0-9]*\s*format\b", re.IGNORECASE)
patt_write = re.compile(r"\s*write\b", re.IGNORECASE)

patt_char6col = re.compile("\s{5,5}\S\s*(.*)")
patt_tabno = re.compile("\t[1-9]\s*(.*)")
patt_amperend = re.compile("(.*)\s*&\s*$", re.DOTALL)
patt_amperstart = re.compile("\s*&\s*(.*)\s*")
patt_blankline = re.compile(r"^\s*$")

patt_sumpi = re.compile(".*/SU_MPI/.*")


def fix_file(file, fix_relationals=0, fudge_format_statement=False, outFileName=None):
    """Process one file at a time"""

    try:
        f = open(file, "r")
    except IOError as err:
        print(f"{file} could not be opened: {err}")
        return 1

    # Read file to memory
    lines = f.readlines()

    # Process lines
    i_line = 0
    routine_found = 0
    # Check if file is include file (no routines)
    while 1:
        if i_line >= len(lines):
            break
        if is_routine(lines[i_line]):
            routine_found = 1
            break
        i_line = i_line + 1

    if not routine_found:  # include file
        i_line = 0
        i_line, is_EOF = fix_routine(i_line, lines, fix_relationals, fudge_format_statement)
        if is_EOF:
            print("EOF")
    else:  # routine file
        while 1:
            if i_line >= len(lines):
                break
            if is_routine(lines[i_line]):
                i_line, is_EOF = fix_routine(i_line, lines, fix_relationals, fudge_format_statement)
                if is_EOF:
                    break
            i_line = i_line + 1

    return write_output(file, lines, outFileName)


def fix_routine(i_line, lines, fix_relationals, fudge_format_statement):
    """Process one routine (until another routine is found)"""

    is_EOF = 0
    implicit_found = 0
    # fix type declaration of functions
    newline, implicit_found = fix_line(lines[i_line], implicit_found, fix_relationals, fudge_format_statement)
    if newline != lines[i_line]:
        lines[i_line] = newline

    i_line = i_line + 1
    i_line = skip_continuation(i_line, lines)
    lines.insert(i_line, use_module_line)

    i_line_use = i_line
    i_line = i_line + 1
    lines = join_lines(i_line, lines)
    while 1:
        if i_line >= len(lines):
            is_EOF = 1
            break
        if is_routine(lines[i_line]):
            i_line = i_line - 1
            break
        newline, implicit_found = fix_line(lines[i_line], implicit_found, fix_relationals, fudge_format_statement)
        # newline=lines[i_line]
        if newline != lines[i_line]:
            # rep(str(i_line-1) + '\n')
            # rep('< ' + lines[i_line])
            # rep('> ' + newline)
            lines[i_line] = newline
        i_line = i_line + 1
    if not implicit_found:
        lines.insert(i_line_use + 1, implicit_complex_line)
        # rep(str(i_line_use+1)+'\n'+'>'+implicit_complex_line)
        i_line = i_line + 1
    return i_line, is_EOF


def write_output(filename, lines, outFileName):
    """Write to output file"""

    head, tail = os.path.split(filename)
    newname = os.path.join(head, "c_" + tail)

    if outFileName is not None:
        newname = outFileName

    try:
        g = open(newname, "w")
    except IOError as err:
        print(f"{newname} could not be opened: {err}")
        return 1
    for line in lines:
        g.write(line)
    g.close()
    return 0


def is_routine(line):
    if patt_program.match(line):
        return 1
    elif patt_module_procedure.match(line):
        return 0
    elif patt_subroutine.match(line):
        return 1
    elif patt_function.search(line):
        return 1
    elif patt_module.match(line):
        return 1
    elif patt_comment.match(line):
        return 0
    else:
        return 0


def join_lines(i, lines):
    # Combine continued lines into one element of
    # the 'lines' array, but keep the '\n's in place.
    # In other words, lines[i] could be more than one
    # line long, but are only one FORTRAN statement.
    while 1:
        i = i + 1
        if i >= len(lines):
            break
        if is_routine(lines[i]):
            break
        if patt_amperend.match(lines[i - 1]) or patt_tabno.match(lines[i]):  # patt_char6col.match(lines[i]) or \
            lines[i - 1] = lines[i - 1] + lines[i]
            del lines[i]
            i = i - 1
        # endif
    return lines


def fix_line(line, implicit_found, fix_relationals, fudge_format_statement):

    # Skip commented lines
    if patt_comment.search(line) != None:
        return (line, implicit_found)

    # Check if we should keep the line.
    if patt_always_real.match(line):
        return (line, implicit_found)

    # Check if we should keep the line.
    if patt_cgns_real.match(line):
        return (line, implicit_found)

    # Check if other lines need to be fixed
    if patt_real.match(line) != None:
        line = fix_real(line)

    ######################
    # if patt_realtype_s.search(line) != None: line = fix_realtype(line)
    # else: print( patt_realtype_s.search(line))
    ######################

    if patt_double.match(line) != None:
        line = fix_double(line)

    if patt_implicit.match(line) != None:
        implicit_found = 1
        line = fix_implicit(line)

    ###if patt_inc.match(line) != None: line = fix_inc(line)
    if fix_relationals:
        if patt_eq.search(line) != None:
            line = patt_eq.sub(r".ceq.", line)
        if patt_ne.search(line) != None:
            line = patt_ne.sub(r".cne.", line)

    if fix_relationals == 2:  # only for MIPS Pro compiler
        if patt_ge.search(line) != None:
            line = patt_ge.sub(r".cge.", line)

    if patt_if.match(line) != None:
        line = fix_if(line)
    elif patt_logic_assignment.match(line) != None:
        line = fix_logic_assignment(line)

    if patt_intrinsic.match(line) != None:
        line = fix_intrinsics(line)

    if patt_mpi_stuff.search(line) != None:
        line = fix_mpi_stuff(line)

    # Assume that this is not needed: CHECK
    # if patt_real_cast.search(line) != None: line = fix_real_cast(line)
    if fudge_format_statement:
        if patt_format.match(line) != None:
            line = fudge_format(line)
        if patt_write.search(line) != None:
            line = fudge_format(line)

    return (line, implicit_found)


def fudge_format(line):
    patt_format_F = re.compile(r"(([0-9]*[ \t]*)((?:F|E)[0-9]+\.[0-9]+))", re.IGNORECASE)
    huh = patt_format_F.findall(line)
    for i in range(len(huh)):
        try:
            rep = str(2 * int(huh[i][1]))
        except ValueError:
            rep = huh[i][1] + "2"
        line = re.sub(huh[i][0], rep + huh[i][2], line)

    return line


def fix_real_cast(line):
    # casting to real in a subroutine call doesn't result
    # in correct implicit cast to complex after that,
    # so make them all explicit casts to complex.
    line = patt_real_cast.sub(r"\1cmplx(", line)
    return line


def fix_mpi_stuff(line):
    # converts everything to double precision
    line = patt_mpi_stuff.sub("MPI_DOUBLE_COMPLEX", line)
    return line


def fix_real(line):
    line = patt_real_s.sub(type_repl, line, 1)
    return line


def fix_double(line):
    space, tail = patt_double.match(line).group(1, 2)
    line = space + "complex*16" + tail + "\n"  # TODO: is \n really needed?
    return line


def fix_implicit(line):
    line = patt_real_s.sub(type_repl, line)
    line = patt_double_s.sub("complex*16", line)
    return line


def fix_if(line):
    # fixes the logical expression inside the if assertion
    # print("_________________________________")
    # print("Fixing if:")
    # print('['+line+']')
    # print("_________________________________")

    preif, ifitself, tmptail = patt_if.match(line).group(1, 2, 3)
    # print('tmptail = ' + tmptail)
    tail = tmptail
    assertion = ""
    count = 1
    for char in tmptail:
        assertion = assertion + char
        tail = tail[1:]
        if char == "(":
            count = count + 1
        if char == ")":
            count = count - 1
        if count == 0:
            assertion = assertion[0:-1]
            break
    assertion = fix_logic_expression(assertion)
    return preif + ifitself + assertion + ")" + tail


def fix_logic_assignment(line):
    # fixes the logical expression on the right hand side of
    # an assignment to a logical variable
    lhs, equals, rhs = patt_logic_assignment.search(line).group(1, 2, 3)
    rhs = re.sub(r"!.*\n", "\n", rhs)  # take out ! style comments
    rhs = fix_logic_expression(rhs[0:-1])  # strip off trailing '\n'

    return lhs + equals + rhs + "\n"


def fix_logic_expression(expression):
    # adds parentheses around user defined .ceq., .cne. in a
    # logical expression containing .and., .or., etc...
    #
    # could be a multi-line line
    # and if the continuation character is '>' or '<' it
    # may screw up the statement parsing, so modify this
    # function with great care.
    #
    # It is assumed that .ceq., .cne., and .cge. were substituted
    # for .eq., .ne., .ge. or ==, /=, >= as necessary.
    #

    # print("_________________________________")
    # print("\tFixing logic expression:")
    # print("\t["+expression+"]")
    # print("_________________________________")

    # patt_connective = re.compile('(\s*\.\s*(?:and|or|not|eqv|neqv)\s*\.[ \t]*)', re.IGNORECASE) # ORIGINAL
    patt_connective = re.compile("(\s*\.\s*(?:and|or|not|eqv|neqv)\s*\.\s*(?:&|)\s*(?:\n|))", re.IGNORECASE)
    patt_operator = re.compile("(\.(?:ceq|cne|cge)\.)", re.IGNORECASE | re.DOTALL)
    split_expression = patt_connective.split(expression)
    if len(split_expression) > 1:
        for i in range(len(split_expression)):
            # print(i, "\t["+split_expression[i]+']')
            strings = patt_operator.split(split_expression[i])
            if len(strings) == 1:
                continue  # no operator, no change
            lhs = strings[0]
            operator = strings[1]
            rhs = strings[2]
            lhs = fix_logic_lhs(lhs)
            rhs = fix_logic_rhs(rhs)
            split_expression[i] = lhs + operator + rhs
            # end for
        expression = "".join(split_expression)  # don't add spaces
    return expression


def fix_logic_lhs(lhs):
    # print('lhs=' + lhs)
    count = 0
    for i in range(len(lhs)):
        j = len(lhs) - (i + 1)  # start from end
        char = lhs[j]
        if char == ")":
            count = count + 1
        if char == "(":
            count = count - 1
        if count == -1:
            j = j + 1
            break
    head = lhs[:j]
    expr = lhs[j:]
    # print('head=' + head + '\t expr=' + expr)
    return head + "(" + expr


def fix_logic_rhs(rhs):
    # print('rhs=' + rhs)
    count = 0
    for i in range(len(rhs)):
        char = rhs[i]
        if char == "(":
            count = count + 1
        if char == ")":
            count = count - 1
        if count == -1:
            i = i - 1
            break
    expr = rhs[: i + 1]
    tail = rhs[i + 1 :]
    return expr + ")" + tail


def fix_intrinsics(line):
    # The following functions appear in the Complexify.f90 module;
    # they are overloaded and thus cannot be declared intrinsic.
    # Keep the list current.
    patt_no_intrin = re.compile(
        r"((?:abs|cosd|acos|sind|asin|atan|atan2|cosh|max|min|sign|dim|sinh|tan|tanh|log10|nint|epsilon)(?:\s*,)?)",
        re.IGNORECASE,
    )
    line = patt_no_intrin.sub("", line)
    # great so far, now need to get rid of empty intrisic statements
    # this next part works only for fixed format files
    patt_char6colB = re.compile(r"\n\s{5,5}\S")
    tmpline = patt_char6colB.sub("", line)  # join lines
    tmpline = tmpline.strip()  # strip away white space
    patt_intrinsicB = re.compile(r"^.*intrinsic\b(?:\s*:\s*:)?$", re.IGNORECASE)
    if patt_intrinsicB.match(tmpline):  # then return empty line
        line = "!     Complexify removed intrinsic line here\n"
    return line


def type_repl(match):
    precision = match.group(1)
    if precision == None:
        precision = "4"
    if eval(precision) == 8:
        data_type = "complex*16"  # double precision complex
    elif eval(precision) == 4:
        data_type = "complex"
    else:
        print(f"Unknown precision: {precision}", file=sys.stderr)
    return data_type


def skip_continuation(i, lines):
    is_continuation = 1
    while is_continuation:
        if i >= len(lines):
            break

        if patt_comment.match(lines[i]):
            is_continuation = 1  # can delete these
        elif patt_char6col.match(lines[i]):
            is_continuation = 1
        elif patt_tabno.match(lines[i]):
            is_continuation = 1
        elif patt_use.match(lines[i]):
            is_continuation = 1
        elif patt_blankline.match(lines[i]):
            is_continuation = 1
        elif patt_amperend.match(lines[i - 1]):
            is_continuation = 1
        else:
            is_continuation = 0
        i = i + 1
    i = i - 1
    return i


use_module_line = "       use complexify \n"
implicit_complex_line = " "


if __name__ == "__main__":
    main()
