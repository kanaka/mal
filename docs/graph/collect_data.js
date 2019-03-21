#!/usr/bin/env python

const { promisify } = require('util')
const readFile = promisify(require('fs').readFile)
const writeFile = promisify(require('fs').writeFile)
const readdir = promisify(require('fs').readdir)
const path = require('path')
const yaml = require('js-yaml')
const request = require('request-promise-native')
const exec = promisify(require('child_process').exec)

const VERBOSE = process.env['VERBOSE'] || false
const BASE_PATH = process.env['BASE_PATH'] || 'base_data.yaml'
const README_PATH = process.env['README_PATH'] || '../../README.md'
// GitHut Pushes
//const GITHUT_URL = 'https://raw.githubusercontent.com/madnight/githut/gh-pages/gh-push-event_eb2696.json'
// GitHut Stars
const GITHUT_URL = process.env['GITHUT_URL'] || 'https://raw.githubusercontent.com/madnight/githut/gh-pages/gh-star-event_e61175.json'
const MAL_PATH = process.env['MAL_PATH'] || '../../'


const githutToNames = {
    'Awk':          ['GNU Awk'],
    'Shell':        ['Bash 4'],
    'JavaScript':   ['JavaScript', 'ES6'],
    'Makefile':     ['GNU Make'],
    'Matlab':       ['MATLAB'],
    'Assembly':     ['NASM'],
    'Pascal':       ['Object Pascal'],
    'Objective-C':  ['Objective C'],
    'PLpgSQL':      ['PL/pgSQL'],
    'PLSQL':        ['PL/SQL'],
    'Scheme':       ['Scheme (R7RS)'],
    'Smalltalk':    ['GNU Smalltalk'],
    'Swift':        ['Swift 2', 'Swift 3', 'Swift 4'],
    'Vim script':   ['Vimscript'],
    'Visual Basic': ['Visual Basic.NET'],
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
    vlog(`Downloading GitHut HTML from '${GITHUT_URL}`)
    const githutText = (await request(GITHUT_URL))
    vlog(`Loading log data from '${logsPath}'`)
    const logFiles = (await readdir(logsPath))
        .map(x => parseInt(x))
        .sort((a, b) => a - b)
    let logData = []
    for (const f of logFiles) {
        if (!(/^[0-9]+$/.exec(f))) { continue }
        const path = logsPath + "/" + f
        logData.push([await readFile(path, 'utf8'), path])
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
                    'star_count':  null,
                    'rank':        null,
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

    vlog(`Processing GitHut data`)
    const gdata = githutText.split(/\n/)
        .map(JSON.parse)
        .filter(d => d.year === "2018" && d.quarter === '4')
        .map(d => (d.count = parseInt(d.count), d))
        .sort((a,b) => (a.count > b.count) ? -1 : a.count < b.count ? 1 : 0)
    let curRank = 1
    for (let gitem of gdata) {
        const names = githutToNames[gitem.name] || [gitem.name]
        for (let name of names) {
            if (name in dataByName) {
                dataByName[name].star_count = gitem.count
                dataByName[name].rank = curRank
                vlog(`  ${dataByName[name].dir} stars: ${gitem.count}, rank: ${curRank}`)
                curRank += 1
            } else {
                vlog(`  ignoring GitHut language ${name}`)
            }
        }
    }

    vlog(`Processing log file data`)
    const perf_run_re = /Running:.*\.\.\/tests\/(perf[0-9])\.mal/
    const perf_num_re = /Elapsed time: ([0-9.]+) msecs|iters over 10 seconds: ([0-9]+)/
    for (let [log, file] of logData) {
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
            vlog(`  ${dir}: ${perfs.perf1}, ${perfs.perf2}, ${perfs.perf3} (perf3 is worse, ignoring ${file})`)
        }
    }

    vlog(`Gathering LOC stats`)
    const stat_re = /SLOC=([0-9]+).*LLOC=([0-9]+).*in ([0-9]+) files/
    process.chdir(MAL_PATH)
    for (let data of dataList) {
        vlog(`  gathering stats information for ${data.dir}`)
        const { stdout, stderr } = await exec(`make "stats^${data.dir}"`)
        const match = stat_re.exec(stdout.split(/\n/)[1])
        data.sloc = parseInt(match[1], 10)
        data.lloc = parseInt(match[2], 10)
        data.files = parseInt(match[3], 10)
    }

    vlog(`Filling in missing attributes`)
    // leave a gap between actual ranked implementations and those
    // with no rankings
    maxRank = curRank + 10
    maxPerf1 = dataList.reduce((a, d) => d.perf1 > a ? d.perf1 : a, 0)
    maxPerf2 = dataList.reduce((a, d) => d.perf2 > a ? d.perf1 : a, 0)
    for (let d of dataList) {
        if (d.rank === null) {
            vlog(`  setting rank to ${maxRank} for ${d.dir}`)
            d.rank = maxRank
        }
        if (d.perf1 === null) {
            vlog(`  setting perf1 to ${maxPerf1} for ${d.dir}`)
            d.perf1 = maxPerf1
        }
        if (d.perf2 === null) {
            vlog(`  setting perf2 to ${maxPerf2} for ${d.dir}`)
            d.perf2 = maxPerf2
        }
    }

    vlog(`Adjusting perf numbers to avoid 0`)
    for (let d of dataList) {
        if (d.perf1 === 0) { d.perf1 = 0.9 }
        if (d.perf2 === 0) { d.perf2 = 0.9 }
        if (d.perf3 === 0) { d.perf3 = 0.01 }
    }

    vlog(`Writing full lanaguage data to ${outPath}`)
    await writeFile(outPath, JSON.stringify(dataByDir, null, 2))

    process.exit(0)
}

main()
