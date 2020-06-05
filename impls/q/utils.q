rl:{1 x; read0 0};

/ We have to get a bit crafty with this one
/ as we cannot really do infinite loops, so
/ we make a iterator that never quits and keeps
/ calling a callback
forever: {{x`; x}/ [{1b}; x]};
