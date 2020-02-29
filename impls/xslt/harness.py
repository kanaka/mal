import time
import os
import readline
import sys
import xml.etree.ElementTree as ET
from threading import Thread
from threading import Lock
from collections import deque

fname = sys.argv[1]
args = sys.argv[2:]
tree = ET.Element('mal')

if len(args) > 0:
    args0 = args[0]
    ET.SubElement(tree, 'argv')
    for a in tree.iter('mal'):
        for a in a.iter('argv'):
            for arg in args[1:]:
                ET.SubElement(a, 'arg').text = arg
    ET.SubElement(tree, 'no_repl')

tree = ET.ElementTree(tree)
stdout = sys.stdout

try:
    readline.read_history_file('.xslt_mal_history')
except:
    pass

finished = False
sem = Lock()
init_t = time.time() * 1000
readline_queue = deque()
os.system('rm -rf xsl_error.xml')
os.system('mkfifo xsl_error.xml')

def setup_request_file():
    os.system('rm -rf xsl_input-string')
    os.system('mkfifo xsl_input-string')


def read_nonblocking(path, bufferSize=100, timeout=.1):
    grace = True
    result = []
    pipe = os.open(path, os.O_RDONLY | os.O_NONBLOCK)
    try:
        while True:
            try:
                buf = os.read(pipe, bufferSize)
                if not buf:
                    break
                else:
                    content = buf.decode("utf-8")
                    line = content.split("\n")
                    result.extend(line)
            except OSError as e:
                if e.errno == 11 and grace:
                    # grace period, first write to pipe might take some time
                    # further reads after opening the file are then successful
                    time.sleep(timeout)
                    grace = False
                else:
                    break

    except OSError as e:
        if e.errno == errno.ENOENT:
            # os.close(pipe)
            pipe = None
        else:
            raise e

    os.close(pipe)
    return result

def serve_one_request():
    res = read_nonblocking('xsl_error.xml', 1024)
    if len(res) == 0:
        return
    for res in res:
        try:
            xtree = ET.fromstring("<data>" + res.strip('\x00') + "</data>")
            # stdout.write(xtree.attrib['kind'])
            for req in xtree:
                if req.attrib['kind'] == 'readline':
                    x = None
                    if len(readline_queue) > 0:
                        x = readline_queue.popleft()
                    else:
                        x = input(req.attrib['value'])
                    with open('xsl_input-string', 'w') as fx:
                        fx.write(x)
                    # stdout.write(' = ' + x)
                elif req.attrib['kind'] == 'display':
                    stdout.write(req.attrib['value'] + '\n')
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
                else:
                    stdout.write("UNKNOWN REQUEST " + req.attrib['kind'])
                # stdout.write('\n')
        except Exception as e:
            # if str(e) != 'no element found: line 1, column 0':
            #     f.seek(0)
            #     print(e, list(x for x in f.read()))
            return
    # with open('xsl_error.xml', 'w') as f:
    #     f.write('')

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
        return
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
    readline_queue.append(f'(load-file "{args0}")')
    transform(do_print=False)
else:
    if fname == 'stepA_mal.xslt':
        readline_queue.append('(println (str "Mal [" *host-language* "]"))')
        transform(do_print=False)
    else:
        transform()
    readline.write_history_file('.xslt_mal_history')

finished = True
th.join()
