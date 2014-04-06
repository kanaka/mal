/* ------------------------------------------------------------------------*
 * Copyright 2013 Arne F. Claassen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0

 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *-------------------------------------------------------------------------*/

var Josh = Josh || {};
(function(root, $, _) {
  Josh.Shell = function(config) {
    config = config || {};

    // instance fields
    var _console = config.console || (Josh.Debug && root.console ? root.console : {
      log: function() {
      }
    });
    var _prompt = config.prompt || 'jsh$';
    var _action = config.action || function(str) {
        return "<div>No action defined for: " + str + "</div>";
    };
    var _shell_view_id = config.shell_view_id || 'shell-view';
    var _shell_panel_id = config.shell_panel_id || 'shell-panel';
    var _input_id = config.input_id || 'shell-cli';
    var _blinktime = config.blinktime || 500;
    var _history = config.history || new Josh.History();
    var _readline = config.readline || new Josh.ReadLine({history: _history, console: _console});
    var _active = false;
    var _cursor_visible = false;
    var _activationHandler;
    var _deactivationHandler;
    var _cmdHandlers = {
      clear: {
        exec: function(cmd, args, callback) {
          $(id(_input_id)).parent().empty();
          callback();
        }
      },
      help: {
        exec: function(cmd, args, callback) {
          callback(self.templates.help({commands: commands()}));
        }
      },
      history: {
        exec: function(cmd, args, callback) {
          if(args[0] == "-c") {
            _history.clear();
            callback();
            return;
          }
          callback(self.templates.history({items: _history.items()}));
        }
      },
      _default: {
        exec: function(cmd, args, callback) {
          callback(self.templates.bad_command({cmd: cmd}));
        },
        completion: function(cmd, arg, line, callback) {
          if(!arg) {
            arg = cmd;
          }
          return callback(self.bestMatch(arg, self.commands()))
        }
      }
    };
    var _line = {
      text: '',
      cursor: 0
    };
    var _searchMatch = '';
    var _view, _panel;
    var _promptHandler;
    var _initializationHandler;
    var _initialized;

    // public methods
    var self = {
      commands: commands,
      templates: {
        history: _.template("<div><% _.each(items, function(cmd, i) { %><div><%- i %>&nbsp;<%- cmd %></div><% }); %></div>"),
        help: _.template("<div><div><strong>Commands:</strong></div><% _.each(commands, function(cmd) { %><div>&nbsp;<%- cmd %></div><% }); %></div>"),
        bad_command: _.template('<div><strong>Unrecognized command:&nbsp;</strong><%=cmd%></div>'),
        input_cmd: _.template('<div id="<%- id %>"><span class="prompt"></span>&nbsp;<span class="input"><span class="left"/><span class="cursor"/><span class="right"/></span></div>'),
        empty_input_cmd: _.template('<div id="<%- id %>"></div>'),
        input_search: _.template('<div id="<%- id %>">(reverse-i-search)`<span class="searchterm"></span>\':&nbsp;<span class="input"><span class="left"/><span class="cursor"/><span class="right"/></span></div>'),
        suggest: _.template("<div><% _.each(suggestions, function(suggestion) { %><div><%- suggestion %></div><% }); %></div>")
      },
      isActive: function() {
        return _readline.isActive();
      },
      activate: function() {
        if($(id(_shell_view_id)).length == 0) {
          _active = false;
          return;
        }
        _readline.activate();
      },
      deactivate: function() {
        _console.log("deactivating");
        _active = false;
        _readline.deactivate();
      },
      setCommandHandler: function(cmd, cmdHandler) {
        _cmdHandlers[cmd] = cmdHandler;
      },
      getCommandHandler: function(cmd) {
        return _cmdHandlers[cmd];
      },
      setPrompt: function(prompt) {
        _prompt = prompt;
        if(!_active) {
          return;
        }
        self.refresh();
      },
      onEOT: function(completionHandler) {
        _readline.onEOT(completionHandler);
      },
      onCancel: function(completionHandler) {
        _readline.onCancel(completionHandler);
      },
      onInitialize: function(completionHandler) {
        _initializationHandler = completionHandler;
      },
      onActivate: function(completionHandler) {
        _activationHandler = completionHandler;
      },
      onDeactivate: function(completionHandler) {
        _deactivationHandler = completionHandler;
      },
      onNewPrompt: function(completionHandler) {
        _promptHandler = completionHandler;
      },
      render: function() {
        var text = _line.text || '';
        var cursorIdx = _line.cursor || 0;
        if(_searchMatch) {
          cursorIdx = _searchMatch.cursoridx || 0;
          text = _searchMatch.text || '';
          $(id(_input_id) + ' .searchterm').text(_searchMatch.term);
        }
        var left = _.escape(text.substr(0, cursorIdx)).replace(/ /g, '&nbsp;');
        var cursor = text.substr(cursorIdx, 1);
        var right = _.escape(text.substr(cursorIdx + 1)).replace(/ /g, '&nbsp;');
        $(id(_input_id) + ' .prompt').html(_prompt);
        $(id(_input_id) + ' .input .left').html(left);
        if(!cursor) {
          $(id(_input_id) + ' .input .cursor').html('&nbsp;').css('textDecoration', 'underline');
        } else {
          $(id(_input_id) + ' .input .cursor').text(cursor).css('textDecoration', 'underline');
        }
        $(id(_input_id) + ' .input .right').html(right);
        _cursor_visible = true;
        self.scrollToBottom();
        _console.log('rendered "' + text + '" w/ cursor at ' + cursorIdx);
      },
      refresh: function() {
        $(id(_input_id)).replaceWith(self.templates.input_cmd({id:_input_id}));
        self.render();
        _console.log('refreshed ' + _input_id);

      },
      println: function(text) {
        var lines = text.split(/\n/);
        for (var i=0; i<lines.length; i++) {
          var line = lines[i];
          if (line == "\\n") {
              continue;
          }
          $(id(_input_id)).after(line);
          $(id(_input_id) + ' .input .cursor').css('textDecoration', '');
          $(id(_input_id)).removeAttr('id');
          $(id(_shell_view_id)).append(self.templates.empty_input_cmd({id:_input_id}));
        }
      },
      scrollToBottom: function() {
        _panel.animate({scrollTop: _view.height()}, 0);
      },
      bestMatch: function(partial, possible) {
        _console.log("bestMatch on partial '" + partial + "'");
        var result = {
          completion: null,
          suggestions: []
        };
        if(!possible || possible.length == 0) {
          return result;
        }
        var common = '';
        if(!partial) {
          if(possible.length == 1) {
            result.completion = possible[0];
            result.suggestions = possible;
            return result;
          }
          if(!_.every(possible, function(x) {
            return possible[0][0] == x[0]
          })) {
            result.suggestions = possible;
            return result;
          }
        }
        for(var i = 0; i < possible.length; i++) {
          var option = possible[i];
          if(option.slice(0, partial.length) == partial) {
            result.suggestions.push(option);
            if(!common) {
              common = option;
              _console.log("initial common:" + common);
            } else if(option.slice(0, common.length) != common) {
              _console.log("find common stem for '" + common + "' and '" + option + "'");
              var j = partial.length;
              while(j < common.length && j < option.length) {
                if(common[j] != option[j]) {
                  common = common.substr(0, j);
                  break;
                }
                j++;
              }
            }
          }
        }
        result.completion = common.substr(partial.length);
        return result;
      }
    };

    function id(id) {
      return "#"+id;
    }

    function commands() {
      return _.chain(_cmdHandlers).keys().filter(function(x) {
        return x[0] != "_"
      }).value();
    }

    function blinkCursor() {
      if(!_active) {
        return;
      }
      root.setTimeout(function() {
        if(!_active) {
          return;
        }
        _cursor_visible = !_cursor_visible;
        if(_cursor_visible) {
          $(id(_input_id) + ' .input .cursor').css('textDecoration', 'underline');
        } else {
          $(id(_input_id) + ' .input .cursor').css('textDecoration', '');
        }
        blinkCursor();
      }, _blinktime);
    }

    function split(str) {
      return _.filter(str.split(/\s+/), function(x) {
        return x;
      });
    }

    function getHandler(cmd) {
      return _cmdHandlers[cmd] || _cmdHandlers._default;
    }

    function renderOutput(output, callback) {
      if(output) {
        $(id(_input_id)).after(output);
      }
      $(id(_input_id) + ' .input .cursor').css('textDecoration', '');
      $(id(_input_id)).removeAttr('id');
      $(id(_shell_view_id)).append(self.templates.input_cmd({id:_input_id}));
      if(_promptHandler) {
        return _promptHandler(function(prompt) {
          self.setPrompt(prompt);
          return callback();
        });
      }
      return callback();
    }

    function activate() {
      _console.log("activating shell");
      if(!_view) {
        _view = $(id(_shell_view_id));
      }
      if(!_panel) {
        _panel = $(id(_shell_panel_id));
      }
      if($(id(_input_id)).length == 0) {
        _view.append(self.templates.input_cmd({id:_input_id}));
      }
      self.refresh();
      _active = true;
      blinkCursor();
      if(_promptHandler) {
        _promptHandler(function(prompt) {
          self.setPrompt(prompt);
        })
      }
      if(_activationHandler) {
        _activationHandler();
      }
    }

    // init
    _readline.onActivate(function() {
      if(!_initialized) {
        _initialized = true;
        if(_initializationHandler) {
          return _initializationHandler(activate);
        }
      }
      return activate();
    });
    _readline.onDeactivate(function() {
      if(_deactivationHandler) {
        _deactivationHandler();
      }
    });
    _readline.onChange(function(line) {
      _line = line;
      self.render();
    });
    _readline.onClear(function() {
      _cmdHandlers.clear.exec(null, null, function() {
        renderOutput(null, function() {
        });
      });
    });
    _readline.onSearchStart(function() {
      $(id(_input_id)).replaceWith(self.templates.input_search({id:_input_id}));
      _console.log('started search');
    });
    _readline.onSearchEnd(function() {
      $(id(_input_id)).replaceWith(self.templates.input_cmd({id:_input_id}));
      _searchMatch = null;
      self.render();
      _console.log("ended search");
    });
    _readline.onSearchChange(function(match) {
      _searchMatch = match;
      self.render();
    });
    _readline.onEnter(function(cmdtext, callback) {
      _console.log("got command: " + cmdtext);
      var result;
      try {
        result = "<div>" + _action(cmdtext) + "</div>";
      } catch (e) {
        result = "<div>" + e.stack + "</div>";
      }
      renderOutput(result, function() {
          callback("");
      });
    });
    _readline.onCompletion(function(line, callback) {
      if(!line) {
        return callback();
      }
      var text = line.text.substr(0, line.cursor);
      var parts = split(text);

      var cmd = parts.shift() || '';
      var arg = parts.pop() || '';
      _console.log("getting completion handler for " + cmd);
      var handler = getHandler(cmd);
      if(handler != _cmdHandlers._default && cmd && cmd == text) {

        _console.log("valid cmd, no args: append space");
        // the text to complete is just a valid command, append a space
        return callback(' ');
      }
      if(!handler.completion) {
        // handler has no completion function, so we can't complete
        return callback();
      }
      _console.log("calling completion handler for " + cmd);
      return handler.completion(cmd, arg, line, function(match) {
        _console.log("completion: " + JSON.stringify(match));
        if(!match) {
          return callback();
        }
        if(match.suggestions && match.suggestions.length > 1) {
          return renderOutput(self.templates.suggest({suggestions: match.suggestions}), function() {
            callback(match.completion);
          });
        }
        return callback(match.completion);
      });
    });
    return self;
  }
})(this, $, _);

var readline = {};
readline.rlwrap = function(action) {
    var history = new Josh.History({ key: 'josh.helloworld'});
    var shell = Josh.Shell({history: history,
                            action: action});
    var promptCounter = 0;
    shell.onNewPrompt(function(callback) {
        promptCounter++;
        callback("user>");
    });
    shell.activate();

    // map output/print to josh.js output
    readline.println = function () {
        shell.println(Array.prototype.slice.call(arguments).join(" "));
    };
}
