import os
import readline
import sys
import xml.etree.ElementTree as ET

fname = sys.argv[1]
tree = ET.Element('mal')
ET.SubElement(tree, 'stdin')
tree = ET.ElementTree(tree)

try:
    readline.read_history_file('.xslt_mal_history')
except:
    pass

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

    tree.write('xslt_input.xml')
    if os.system(f'saxon -xsl:"{fname}" -s:xslt_input.xml > xslt_output.xml 2> xsl_error.xml'):
        with open('xsl_error.xml', 'r') as f:
            print(f.readlines()[0])
        continue
    else:
        with open('xsl_error.xml', 'r') as f:
            print(f.read(), end='')

    tree = ET.parse('xslt_output.xml')
    stdout = ''
    for a in tree.iter('mal'):
        for a in a.iter('stdout'):
            stdout = a
    print(stdout.text)
    stdout.clear()
    del stdout

readline.write_history_file('.xslt_mal_history')
