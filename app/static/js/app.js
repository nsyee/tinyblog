var MV = {};

MV.getEntries = function() {
    $.ajax({
        type    : 'GET',
        url     : '/json/entries',
        data    : 'count=8',
        dataType: 'json',
        success : function(data) {
            showNavi(data);
            showRecent(data);

            function showNavi(data) {
                var currentEntry = $('#entrycode').text(),
                    i, sz, current, prev, next;

                for (i=0, sz=data.length; i<sz; ++i) {
                    if (data[i].code === currentEntry) {
                        current = i;
                        break;
                    }
                }
                prev = current+1;
                next = current-1;

                if (current == sz-1) {
                    $('.prev').css('visibility', 'hidden');
                    prev = current;
                }
                if (current == 0) {
                    $('.next').css('visibility', 'hidden');
                    next = current;
                }

                $('#nav').render(data, {
                    '.prev a+'    : function(arg) { return arg.data[prev].title; },
                    '.prev a@href': function(arg) { return arg.data[prev].url; },
                    '+.next a'    : function(arg) { return arg.data[next].title; },
                    '.next a@href': function(arg) { return arg.data[next].url; }
                })
            }

            function showRecent(data) {
                var RECENT_MAX = 8;
                data = {entries: data.slice(0, RECENT_MAX)};
                $('#recent-entry ul').render(data, {
                    'li': {
                        'entry<-entries': {
                            'a'     : 'entry.title',
                            'a@href': 'entry.url'
                        }
                    }
                });
            }
        }
    });
}

MV.init = function() {
    this.getEntries();
}

$(function() {
    MV.init();
});
