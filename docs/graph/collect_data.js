#!/usr/bin/env python

const { promisify } = require('util')
const readFile = promisify(require('fs').readFile)
const writeFile = promisify(require('fs').writeFile)
const readdir = promisify(require('fs').readdir)
const path = require('path')
const yaml = require('js-yaml')
const csv = require('csvtojson')
const request = require('request-promise-native')
const exec = promisify(require('child_process').exec)

const VERBOSE = process.env['VERBOSE'] || false
const BASE_PATH = process.env['BASE_PATH'] || 'base_data.yaml'
const README_PATH = process.env['README_PATH'] || '../../README.md'
const MAL_PATH = process.env['MAL_PATH'] || '../../'

// GitHut 2.0 Pull Requests
const GITHUT_PULL_URL = process.env['GITHUT_PULL_URL'] || 'https://raw.githubusercontent.com/madnight/githut/master/src/data/gh-pull-request.json'
// GitHut 2.0 Pushes
const GITHUT_PUSH_URL = process.env['GITHUT_PUSH_URL'] || 'https://raw.githubusercontent.com/madnight/githut/master/src/data/gh-push-event.json'
// GitHut 2.0 Stars
const GITHUT_STAR_URL = process.env['GITHUT_STAR_URL'] || 'https://raw.githubusercontent.com/madnight/githut/master/src/data/gh-star-event.json'

// Refresh this link using this Query page:
// https://data.stackexchange.com/stackoverflow/query/edit/1013465
const SO_TAG_CSV_URL = process.env['SO_TAG_CSV_URL'] || 'https://data.stackexchange.com/stackoverflow/csv/1252107'


const githutToNames = {
    'Awk':          ['GNU Awk'],
    'Ada':          ['Ada', 'Ada #2'],
    'Shell':        ['Bash 4'],
    'JavaScript':   ['JavaScript', 'ES6'],
    'Makefile':     ['GNU Make'],
    'Matlab':       ['MATLAB'],
    'Assembly':     ['NASM'],
    'Pascal':       ['Object Pascal'],
    'Objective-C':  ['Objective C'],
    'PLpgSQL':      ['PL/pgSQL'],
    'PLSQL':        ['PL/SQL'],
    'Python':       ['Python', 'Python #2'],
    'Scheme':       ['Scheme (R7RS)'],
    'Smalltalk':    ['GNU Smalltalk'],
    'Swift':        ['Swift 2', 'Swift 3', 'Swift 4', 'Swift 5'],
    'Vim script':   ['Vimscript'],
    'Visual Basic': ['Visual Basic.NET'],
}
const dirToSOTags = {
    'ada.2':     ['ada'],
    'bbc-basic': ['bbc-micro'],
    'cpp':       ['c++', 'c++98', 'c++11', 'c++14', 'c++17'],
    'coffee':    ['coffeescript'],
    'crystal':   ['crystal-lang'],
    'cs':        ['c#', 'c#-2.0', 'c#-3.0', 'c#-4.0'],
    'es6':       ['ecmascript-6', 'es6-promise', 'es6-modules', 'es6-class', 'reactjs'],
    'fsharp':    ['f#', 'f#-interactive', 'f#-data', 'f#-3.0'],
    'factor':    ['factor-lang'],
    'js':        ['javascript', 'node.js', 'jquery', 'angular'],
    'logo':      ['logo-lang'],
    'make':      ['makefile'],
    'objpascal': ['delphi', 'freepascal', 'delphi-7', 'delphi-2007', 'delphi-2009', 'delphi-2010', 'delphi-xe', 'delphi-xe2', 'delphi-xe3', 'delphi-xe4', 'delphi-xe5', 'delphi-xe7'],
    'objc':      ['objective-c'],
    'python':    ['python', 'python-3.x', 'python-2.7'],
    'python.2':  ['python', 'python-3.x', 'python-2.7'],
    'swift':     ['swift2'],
    'swift3':    ['swift3'],
    'swift4':    ['swift4'],
    'swift5':    ['swift', 'swift4', 'swift5'],
    'ts':        ['typescript', 'typescript-generics', 'typescript2.0'],
    'vimscript': ['viml'],
    'vb':        ['vb.net'],
    'wasm':      ['webassembly'],
}

function vlog(...args) {
    if (VERBOSE) {
        console.log(...args)
    }
}

function die(code, ...args) {
    console.error(...args)
    process.exit(code)
}

async function main() {
    const logsPath = path.resolve(process.argv[2])
    const outPath = path.resolve(process.argv[3])

    vlog(`Loading base data yaml from '${BASE_PATH}`)
    const baseYaml = yaml.safeLoad(await readFile(BASE_PATH, 'utf8'))
    vlog(`Loading README text from '${README_PATH}`)
    const readmeLines = (await readFile(README_PATH, 'utf8')).split(/\n/)
    vlog(`Downloading GitHut Pulls HTML from '${GITHUT_PULL_URL}`)
    const githutPullText = (await request(GITHUT_PULL_URL))
    vlog(`Downloading GitHut Pushes HTML from '${GITHUT_PUSH_URL}`)
    const githutPushText = (await request(GITHUT_PUSH_URL))
    vlog(`Downloading GitHut Stars HTML from '${GITHUT_STAR_URL}`)
    const githutStarText = (await request(GITHUT_STAR_URL))
    vlog(`Downloading StackOverflow Tag CSV from '${SO_TAG_CSV_URL}`)
    const soTagList = await csv().fromStream(request.get(SO_TAG_CSV_URL))
    vlog(`Loading log data from '${logsPath}'`)
    const logFiles = (await readdir(logsPath))
        .map(x => parseInt(x))
        .sort((a, b) => a - b)
    let logData = []
    for (const f of logFiles) {
        if (!(/^[0-9]+$/.exec(f))) { continue }
        const path = logsPath + "/" + f
        logData.push([await readFile(path, 'utf8'), path, f])
    }

    let dirs = []
    let names = []
    let dataList = []
    let dataByDir  = {}
    let dataByName = {}

    vlog(`Processing base data`)
    for (let d of baseYaml['languages']) {
        let data = {'dir':         d[0],
                    'name':        d[1],
                    'syntax':      d[2],
                    'type_check':  d[3],
                    'modes':       d[4],
                    'perf1':       null,
                    'perf2':       null,
                    'perf3':       0,
                    'pull_count':  null,
                    'pull_rank':   null,
                    'push_count':  null,
                    'push_rank':   null,
                    'star_count':  null,
                    'star_rank':   null,
                    'sloc':        0,
                    'files':       0}
        dirs.push(d[0])
        names.push(d[1])
        dataList.push(data)
        dataByDir[d[0]]  = data
        dataByName[d[1]] = data
    }


    vlog(`Processing README implementations table`)
    const readme_re = /^\| \[([^\[]*)\].* \| \[([^|]*)\]\(([^|]*)\)  *\| *$/
    for (let row of readmeLines.filter(l => /^\| [\[]/.exec(l))) {
        t = readme_re.exec(row)
        if (t) {
            if (t[1] in dataByName) {
                let data = dataByName[t[1]]
                data.author_name = t[2]
                data.author_url  = t[3]
            } else {
                die(1, `README language '${t[1]}' not found in base data`)
            }
        } else {
            die(1, `No match for README table row: ${row}`)
        }
    }


    function githutProcess(textData, kind) {
        const gMap = JSON.parse(textData)
            .reduce((m, d) => (m[d.name] = parseInt(d.count) + (m[d.name] || 0), m), {})
        const gdata = Object.entries(gMap)
            .sort(([k1,v1],[k2,v2]) => v2 - v1)
        let curRank = 1
        for (let [gname, gcount] of gdata) {
            const names = githutToNames[gname] || [gname]
            for (let name of names) {
                if (name in dataByName) {
                    dataByName[name][kind + '_count'] = gcount
                    dataByName[name][kind + '_rank'] = curRank
                    vlog(`  ${dataByName[name].dir} count: ${gcount}, rank: ${curRank}`)
                    curRank += 1
                } else {
                    vlog(`  ignoring GitHut language ${name}`)
                }
            }
        }
        return curRank;
    }
    vlog(`Processing GitHut Pull Request data`)
    githutProcess(githutPullText, 'pull')
    vlog(`Processing GitHut Push data`)
    githutProcess(githutPushText, 'push')
    vlog(`Processing GitHut Stars data`)
    githutProcess(githutStarText, 'star')


    vlog(`Processing StackOverflow tag data`)
    const soMap = soTagList
        //.map(d => ({tag: d.TagName, count: parseInt(d.Rate)}))
        //.sort((a,b) => b.count - a.count)
        .reduce((m,d) => (m[d.TagName] = parseInt(d.Rate), m), {})
    soMap['mal'] = 0 // NOTE/TODO: StackOverflow mal is something else
    for (let dir of dirs) {
        if (!('so_count' in dataByDir[dir])) {
            dataByDir[dir]['so_count'] = 0
        }
        let tags = dirToSOTags[dir]
        if (!tags) {
            if (dir in soMap) {
                tags = [dir]
            } else {
                vlog(`  ${dir} not found as StackOverflow tag`)
                tags = []
            }
        }
        for (let tag of tags) {
            if (tag in soMap) {
                dataByDir[dir]['so_count'] += soMap[tag]
                //vlog(`  ${dir} count: ${count}`)
            } else {
                die(1, `${tag} not found in soMap`)
            }
        }
    }
    vlog()
    let curRank = 1
    let soSort = Object.values(dataByDir).sort((a,b) => b.so_count - a.so_count)
    for (let data of soSort) {
        data.so_rank = curRank
        vlog(`  ${data.dir} so_count: ${data.so_count}, rank: ${curRank}`)
        curRank += 1
    }
    const maxSORank = curRank


    vlog(`Processing log file data`)
    const perf_run_re = /Running:.*\.\.\/tests\/(perf[0-9])\.mal/
    const perf_num_re = /Elapsed time: ([0-9.]+) msecs|iters over 10 seconds: ([0-9]+)/
    for (let [log, file, idx] of logData) {
        const dir_match = (/export IMPL=(\S+)/i).exec(log)
        if (!dir_match) { die(1, `no IMPL found in ${file}`) }
        const dir = dir_match[1]
        const data = dataByDir[dir]
//        if (data.perf1 !== null) {
//            vlog(`  ${dir} already has perf data, ignoring ${file}`)
//            continue
//        }
        const perfs = {}
        const logLines = log.split(/\n/)
        for (let i = 0; i < logLines.length; i++) {
            const match_run = perf_run_re.exec(logLines[i])
            if (match_run) {
                // Find the result line
                let match_num = null
                do {
                    i += 1
                    match_num = perf_num_re.exec(logLines[i])
                    if (match_num) {
                        num = parseFloat(match_num[1] || match_num[2], 10)
                        perfs[match_run[1]] = num
                    }
                } while ((!match_num) && i < logLines.length)
            }
        }
        if ((perfs.perf3 > data.perf3) || !data.perf3) {
            data.perf1 = perfs.perf1
            data.perf2 = perfs.perf2
            data.perf3 = perfs.perf3
            vlog(`  ${dir}: ${perfs.perf1}, ${perfs.perf2}, ${perfs.perf3}`)
        } else {
            vlog(`  ${dir}: ${perfs.perf1}, ${perfs.perf2}, ${perfs.perf3} (perf3 is worse, ignoring log ${idx})`)
        }
    }


    vlog(`Gathering LOC stats`)
    const stat_re = /SLOC=([0-9]+).*LLOC=([0-9]+).*in ([0-9]+) files/
    process.chdir(MAL_PATH)
    for (let data of dataList) {
        const { stdout, stderr } = await exec(`make "stats^${data.dir}"`)
        const match = stat_re.exec(stdout.split(/\n/)[1])
        data.sloc = parseInt(match[1], 10)
        data.lloc = parseInt(match[2], 10)
        data.files = parseInt(match[3], 10)
        vlog(`  ${data.dir}: sloc: ${data.sloc}, lloc: ${data.lloc}, files: ${data.files}`)
    }


    vlog(`Writing full lanaguage data to ${outPath}`)
    await writeFile(outPath, JSON.stringify(dataByDir, null, 2))

    process.exit(0)
}

main()
