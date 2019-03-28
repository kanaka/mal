const malColors = [
    "#1f77b4","#bf7f0e","#4cb00c","#b62728","#9467bd","#bc664b","#b377c2","#0fbf6f","#bcbd22","#17beef",
    "#1f6784","#8f7f0e","#4c800c","#862728","#54678d","#8c564b","#8377c2","#0f8f6f","#8c8d22","#178eef",
    "#1f97d4","#ff7f0e","#4cf00c","#f62728","#c467fd","#fc764b","#f377c2","#0fff6f","#fcfd22","#17feef",
]

const axisMap = {
    'pull_rank':  'GH PRs',
    'push_rank':  'GH Pushes',
    'star_rank':  'GH Stars',
    'so_rank':    'SO Tags',
    'perf1':      'Perf 1',
    'perf2':      'Perf 2',
    'perf3':      'Perf 3',
    'sloc':       'SLOC size',
    'files':      'File count',
}
const colorMap = {
    'syntax': 'Syntax Style',
    'type_check': 'Type Discipline',
    'author_name': 'Author',
}
const axisKeySet = new Set(Object.keys(axisMap))
const colorKeySet = new Set(['type_check', 'syntax', 'author_name'])

const perfSet = new Set(['perf1', 'perf2', 'perf3'])
const invertSet = new Set(['pull_rank', 'push_rank', 'star_rank', 'so_rank', 'perf1', 'perf2'])
const perfLogSet = new Set(['perf1', 'perf2', 'sloc', 'files'])

let cfg = {
    ckey: 'syntax',
    xkey: 'push_rank',
    ykey: 'perf3',
    skey: 'sloc',

    xlog: false,
    ylog: true,
}

let allData
let graphData = []
let chart

//
// Util functions
//

function malExtent(data, key) {
    let extent = d3.extent(Object.values(data), d => d[key])
    // pad the bottom rank so it's not on the opposite axis line
    if (key.endsWith('_rank')) {
        extent[0] = 0.99 // Setting this to 1 breaks log scale render
        extent[extent.length-1] += 1
    }
    // Replace 0's with 0.01 to prevent divide by zero errors
    if (extent[0] === 0) { extent[0] = 0.0001 }
    if (extent[extent.length-1] === 0) { extent[extent.length-1] = 0.0001 }
    // For rankings, perf1, and perf2 reverse the Axis range
    if (invertSet.has(key)) {
        extent.reverse()
    }
    return extent
}

function malScale(log) {
    return log ? d3.scale.log() : d3.scale.linear()
}

function malTickValues(key, log) {
    if (log && perfSet.has(key)) {
        return [1, 10, 100, 1000, 10000, 100000]
    } else {
        return null
    }
}

function malCircleSize(key, min, max, val) {
    let size = (val || 0.01) - (min - 0.01)
    if (invertSet.has(key)) {
        size = (max + 0.01) - size
    }
//    if (perfLogSet.has(key)) {
//        size = Math.log(size)
//    }
//    console.log(key, max, val, size)
    return size
}


//
// UI / Axis Data / query parameters
//

// Parser query string and update cfg map with valid config options
(function parseQuery(q) {
    const pairs = (q[0] === '?' ? q.substr(1) : q).split('&')
    for (const [p1, p2] of pairs.map(p => p.split('='))) {
        let k = decodeURIComponent(p1).toLowerCase()
        let v = p2 ? decodeURIComponent(p2) : true
        if (v in {"true":1,"1":1,"yes":1}) { v = true }
        if (v in {"false":1,"0":1,"no":1}) { v = false }
        if (k in cfg && (axisKeySet.has(v) || colorKeySet.has(v))) {
            cfg[k] = v
        }
        if ((new Set(['xlog', 'ylog'])).has(k) && typeof v === 'boolean') {
            cfg[k] = v
        }
    }
})(location.search)

// Generate the control buttons and set the checked elements based on
// the cfg
function ctlChange(evt) {
    if (new Set(['xlog', 'ylog']).has(evt.target.name)) {
        cfg[evt.target.name] = evt.target.checked
    } else {
        cfg[evt.target.name] = evt.target.value
    }
    const query = Object.entries(cfg).map(([k,v]) => k + "=" + v).join('&')
    history.pushState(null, '', '?' + query)
    updateGraphData()
}
for (let key of ['ckey', 'xkey', 'ykey', 'skey']) {
    const parent = document.getElementById(key + '-controls')
    const ctlMap = ({
        'ckey': colorMap,
        'xkey': Object.assign({}, axisMap, {'xlog': 'Log Scale'}),
        'ykey': Object.assign({}, axisMap, {'ylog': 'Log Scale'}),
        'skey': axisMap,
    })[key]
    for (let [val, name] of Object.entries(ctlMap)) {
        const log = (new Set(['xlog', 'ylog']).has(val)) ? val : false
        const ctl = document.createElement('input')
        ctl.class = 'selects'
        ctl.type = log ? 'checkbox' : 'radio'
        ctl.name = log ? log : key
        ctl.value = log ? true : val
        if ((log && cfg[val] === true) || cfg[key] === val) {
            ctl.checked = true
        }
        ctl.addEventListener('change', ctlChange)
        parent.appendChild(ctl)
        parent.appendChild(document.createTextNode(name))
    }
}

//
// Graph rendering / updating
//

function updateGraphData() {
    let xMax = 0
    let yMax = 0
    let sMin = null
    let sMax = null
    const colorSet = new Set(Object.values(allData).map(d => d[cfg.ckey]))
    const colorList = Array.from(colorSet.values())
    // empty the graphData without recreating it
    while (graphData.length > 0) { graphData.pop() }
    graphData.push(...colorList.map(t => ({key: t, values: []})))
    for (let dir of Object.keys(allData)) {
        const impl = allData[dir]
        if (impl[cfg.xkey] > xMax) { xMax = impl[cfg.xkey] }
        if (impl[cfg.ykey] > yMax) { yMax = impl[cfg.ykey] }
        if (sMin === null) { sMin = impl[cfg.skey] }
        if (impl[cfg.skey] < sMin) { sMin = impl[cfg.skey] }
        if (impl[cfg.skey] > sMax) { sMax = impl[cfg.skey] }
    }
    for (let dir of Object.keys(allData)) {
        const impl = allData[dir]
        // Invert size for inverted data
        graphData[colorList.indexOf(impl[cfg.ckey])].values.push({
            x: impl[cfg.xkey] || 0,
            y: impl[cfg.ykey] || 0,
            size: malCircleSize(cfg.skey, sMin, sMax, impl[cfg.skey]),
            shape: 'circle',
            label: impl.name,
            impl: impl,
        })
    }

    // Update the axes domain, scale and tick values
    chart.xDomain(malExtent(allData, cfg.xkey))
    chart.yDomain(malExtent(allData, cfg.ykey))
    chart.xScale(malScale(cfg.xlog))
    chart.yScale(malScale(cfg.ylog))
    chart.xAxis.tickValues(malTickValues(cfg.xkey, cfg.xlog))
    chart.yAxis.tickValues(malTickValues(cfg.ykey, cfg.ylog))
    chart.xAxis.axisLabel(axisMap[cfg.xkey])
    chart.yAxis.axisLabel(axisMap[cfg.ykey])

    // Update the graph
    d3.select('#mal svg')
        .data([graphData])
        .transition().duration(350).ease('linear')
        .call(chart)

    chart.update()

    nv.utils.windowResize(chart.update)
}

nv.addGraph(function() {
    chart = nv.models.scatterChart()
        .showDistX(true)
        .showDistY(true)
        .showLabels(true)
        .duration(300)
        .color(malColors)
    chart.dispatch.on('renderEnd', function() {
        //console.log('render complete')
    })
    chart.dispatch.on('stateChange', function(e) {
        nv.log('New State:', JSON.stringify(e))
    })
    chart.tooltip.contentGenerator(function(obj) {
        const i = obj.point.impl
        return '<h3>' + i.name + '</h3>' +
            '<ul class="impl-data">' +
            '<li><b>Syntax Style</b>: ' + i.syntax + '<br>' +
            '<li><b>Type Discipline</b>: ' + i.type_check + '<br>' +
            '<li><b>GitHub</b>:' +
            '  <ul>' +
            '  <li><b>PR Count</b>: ' + (i.pull_count || 'unknown') + '<br>' +
            '  <li><b>PR Rank</b>: ' + i.pull_rank + '<br>' +
            '  <li><b>Push Count</b>: ' + (i.push_count || 'unknown') + '<br>' +
            '  <li><b>Push Rank</b>: ' + i.push_rank + '<br>' +
            '  <li><b>Star Count</b>: ' + (i.star_count || 'unknown') + '<br>' +
            '  <li><b>Star Rank</b>: ' + i.star_rank + '<br>' +
            '  </ul>' +
            '<li><b>StackOverflow</b>:' +
            '  <ul>' +
            '  <li><b>Tag Count</b>: ' + (i.so_count || 'unknown') + '<br>' +
            '  <li><b>Tag Rank</b>: ' + i.so_rank + '<br>' +
            '  </ul>' +
            '<li><br>' +
            '<li><b>Perf 1</b>: ' + i.perf1 + ' ms<br>' +
            '<li><b>Perf 2</b>: ' + i.perf2 + ' ms<br>' +
            '<li><b>Perf 3</b>: ' + i.perf3 + ' iters / 10 sec<br>' +
            '<li><b>SLOC</b>: ' + i.sloc + ' lines<br>' +
            '<li><b>Author</b>: ' + i.author_name + '<br>' +
            '&nbsp; &nbsp; ' + i.author_url.replace(/https?:\/\//, '') + '<br>' +
            '</ul>'
    })

    // Load and mangle the data
    d3.json("all_data.json", function (error, data) {
        allData = data

        console.log(`Filling in missing data attributes`)
        const dataList = Object.values(allData)
        // leave a gap between ranked impls and those with no rank
        const rankGap = 10
        const maxPullRank = Math.max(...dataList.map(d => d.pull_rank))
        const maxPushRank = Math.max(...dataList.map(d => d.push_rank))
        const maxStarRank = Math.max(...dataList.map(d => d.star_rank))
        const maxSORank = Math.max(...dataList.map(d => d.so_rank))
        const maxPerf1 = dataList.reduce((a, d) => d.perf1 > a ? d.perf1 : a, 0)
        const maxPerf2 = dataList.reduce((a, d) => d.perf2 > a ? d.perf1 : a, 0)
        for (let d of dataList) {
            if (d.pull_rank === null) {
                d.pull_rank = maxPullRank + rankGap
                console.log(`  set pull_rank to ${d.pull_rank} for ${d.dir}`)
            }
            if (d.push_rank === null) {
                d.push_rank = maxPushRank + rankGap
                console.log(`  set push_rank to ${d.push_rank} for ${d.dir}`)
            }
            if (d.star_rank === null) {
                d.star_rank = maxStarRank + rankGap
                console.log(`  set star_rank to ${d.star_rank} for ${d.dir}`)
            }
            if (d.so_count === 0) {
                d.so_rank = maxSORank + rankGap
                console.log(`  set so_rank to ${d.so_rank} for ${d.dir}`)
            }
            if (d.perf1 === null) {
                d.perf1 = maxPerf1
                console.log(`  set perf1 to ${maxPerf1} for ${d.dir}`)
            }
            if (d.perf2 === null) {
                d.perf2 = maxPerf2
                console.log(`  set perf2 to ${maxPerf2} for ${d.dir}`)
            }
        }

        console.log(`Adjusting perf numbers to avoid 0`)
        for (let d of dataList) {
            if (d.perf1 === 0) { d.perf1 = 0.9 }
            if (d.perf2 === 0) { d.perf2 = 0.9 }
            if (d.perf3 === 0) { d.perf3 = 0.01 }
        }

        // NOTE: TODO: major hack to workaround bug with switching
        // to/from logarithmic mode. Seems to require at least one
        // value to be less than 1 for it to work
        allData.rpython.perf2 = 0.9

        updateGraphData()
    })

    return chart
})

