import os
import readline
import sys
import xml.etree.ElementTree as ET

fname = sys.argv[1]
args = sys.argv[2:]
tree = ET.Element('mal')
ET.SubElement(tree, 'stdin')

if len(args) > 0:
    args0 = args[0]
    ET.SubElement(tree, 'argv')
    for a in tree.iter('mal'):
        for a in a.iter('argv'):
            for arg in args[1:]:
                ET.SubElement(a, 'arg').text = arg

tree = ET.ElementTree(tree)

try:
    readline.read_history_file('.xslt_mal_history')
except:
    pass

def transform(do_print=True):
    global tree

    tree.write('xslt_input.xml')
    if os.system(f'saxon -xsl:"{fname}" -s:xslt_input.xml > xslt_output.xml 2> xsl_error.xml'):
        with open('xsl_error.xml', 'r') as f:
            print('Error:', [x for x in f.readlines() if x.strip() != ''][-1], end='')
        return
    else:
        with open('xsl_error.xml', 'r') as f:
            print(f.read(), end='')

    tree = ET.parse('xslt_output.xml')
    if do_print:
        stdout = ''
        for a in tree.iter('mal'):
            for a in a.iter('stdout'):
                stdout = a
        print(stdout.text)
        stdout.clear()
        del stdout


if len(args) > 0:
    for a in tree.iter('mal'):
        for a in tree.iter('stdin'):
            a.text = f'(load-file "{args0}")'
    transform(False)
else:
    while True:
        try:
            x = input('user> ')
        except EOFError:
            break
        except KeyboardInterrupt:
            break

        for a in tree.iter('mal'):
            for a in a.iter('stdin'):
                a.text = x
        
        transform()

    readline.write_history_file('.xslt_mal_history')
