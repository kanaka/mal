var max_history_length = 1000;

function jq_load_history(jq) {
    if (localStorage['mal_history']) {
        var lines = JSON.parse(localStorage['mal_history']);
        if (lines.length > max_history_length) {
            lines = lines.slice(lines.length-max_history_length);
        }
        jq.SetHistory(lines);
    }
}

function jq_save_history(jq) {
    var lines = jq.GetHistory();
    localStorage['mal_history'] = JSON.stringify(lines);
}


var readline = {
    'readline': function(prompt_str) {
            return prompt(prompt_str);
        }};

