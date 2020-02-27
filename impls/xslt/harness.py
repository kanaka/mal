import time
import os
import readline
import sys
import xml.etree.ElementTree as ET
from threading import Thread
from threading import Lock

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
stdout = sys.stdout

try:
    readline.read_history_file('.xslt_mal_history')
except:
    pass

finished = False
sem = Lock()
init_t = time.time() * 1000
def setup_request_file():
    os.system('rm -rf xsl_input-string')
    os.system('mkfifo xsl_input-string')

def serve_one_request():
    with open('xsl_error.xml', 'r') as f:
        try:
            xtree = ET.fromstring(f.read().strip('\x00'))
            # stdout.write(xtree.attrib['kind'])
            req = xtree
            if req is not None:
                if req.attrib['kind'] == 'readline':
                    stdout.write(req.attrib['value'])
                    x = input()
                    with open('xsl_input-string', 'w') as fx:
                        fx.write(x)
                    # stdout.write(' = ' + x)
                elif req.attrib['kind'] == 'display':
                    x = req.attrib['value']
                    # stdout.write(' = ' + x)
                    stdout.write(x + '\n')
                elif req.attrib['kind'] == 'time':
                    x = time.time() * 1000 - init_t
                    # stdout.write(' = ' + str(int(x)))
                    with open('xsl_input-string', 'w') as fx:
                        fx.write(str(int(x)))
                # stdout.write('\n')
                elif req.attrib['kind'] == 'xpath-eval':
                    xpath = req.attrib['value']
                    with open('xsl-eval.xslt', 'w') as f:
                        f.write(f'<?xml version="1.0" encoding="UTF-8"?><xsl:stylesheet version="3.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"  xmlns:xs="http://www.w3.org/2001/XMLSchema"  xmlns:map="http://www.w3.org/2005/xpath-functions/map" xmlns:env="ENV" xmlns:core="CORE" exclude-result-prefixes="env core xs xsl map fn"><xsl:output omit-xml-declaration="yes"/><xsl:template match="/"><xsl:sequence select="{xpath}" /></xsl:template></xsl:stylesheet>')
                    with open('xsl-null.xml', 'w') as f:
                        f.write(req.attrib['context'])

                    if os.system(f'saxon -xsl:xsl-eval.xslt -s:xsl-null.xml > xsl-eval_output.xml'):
                        x = ''
                    else:
                        with open('xsl-eval_output.xml', 'r') as f:
                            x = f.read()
                    with open('xsl_input-string', 'w') as fx:
                        fx.write(x)
                # stdout.write('\n')
        except Exception as e:
            # if str(e) != 'no element found: line 1, column 0':
            #     f.seek(0)
            #     print(e, list(x for x in f.read()))
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
            # print(e)
            pass


th = Thread(target=serve_requests)
th.start()

def transform(do_print=True):
    global tree

    tree.write('xslt_input.xml')
    if os.system(f'saxon -xsl:"{fname}" -s:xslt_input.xml -TP:perf.html > xslt_output.xml 2> xsl_error.xml'):
        with open('xsl_error.xml', 'r') as f:
            lines = f.readlines()
            if len(lines):
                print('Error:', [x for x in lines if x.strip() != ''][-1], end='')
        return
    else:
        try:
            with open('xsl_error.xml', 'r') as f:
                print(f.read(), end='')
        except:
            # nothing interesting happened
            # HOW?
            pass

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
    transform(do_print=False)
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
