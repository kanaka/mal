import os
import readline
import sys
import xml.etree.ElementTree as ET
from threading import Thread

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

finished = False

def setup_request_file():
    os.system('rm -rf xsl_input-string')
    os.system('mkfifo xsl_input-string')

def serve_one_request():
    with open('xsl_error.xml', 'r') as f:
        try:
            xtree = ET.parse(f)
            for req in xtree.iter('request'):
                if req.attrib['kind'] == 'readline':
                    x = input(req.attrib['value'])
                    with open('xsl_input-string', 'w') as fx:
                        fx.write(x)
        except Exception as e:
            return
    with open('xsl_error.xml', 'w') as f:
        f.write('')

def serve_requests():
    global finished
    setup_request_file()
    while not finished:
        try:
            serve_one_request()
        except Exception as e:
            print(e)


th = Thread(target=serve_requests)
th.start()

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
    if fname == 'stepA_mal.xslt':
        # prepare state
        for a in tree.iter('mal'):
                for a in a.iter('stdin'):
                    a.text = '(println (str "Mal [" *host-language* "]"))'
        transform(do_print=False)

    # repl loop
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

finished = True
th.join()