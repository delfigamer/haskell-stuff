local source = io.open('concept.txt'):read('*a')
local target = ''

local kwset = 'lua'
-- local kwset = 'kon'
-- local kwset = 'hs'

local getch
local ungetch
local peekch
do
    local pos = 1

    function getch()
        if pos <= #source then
            local ch = string.sub(source, pos, pos)
            pos = pos + 1
            return ch
        else
            pos = pos + 1
            return ''
        end
    end

    function ungetch(ch)
        pos = pos - 1
    end

    function peekch()
        if pos <= #source then
            local ch = string.sub(source, pos, pos)
            return ch
        else
            return ''
        end
    end
end

local write
local setformat
do
    local style = nil
    local isbold = nil

    function write(...)
        local args = {...}
        for i = 1, select('#', ...) do
            local arg = args[i]
            if arg then
                target = target .. tostring(arg)
            end
        end
    end

    function setformat(newstyle, newisbold)
        if style ~= newstyle then
            if isbold then
                write('</b>')
            end
            if style then
                write('</span>')
            end
            if newstyle then
                write('<span style="', newstyle, '">')
            end
            if newisbold then
                write('<b>')
            end
        elseif isbold ~= newisbold then
            if isbold then
                write('</b>')
            end
            if newisbold then
                write('<b>')
            end
        end
        style = newstyle
        isbold = newisbold
    end
end

local function setformat_string()
    return setformat('\z
        color:#FF0000')
end

local function setformat_longstring()
    return setformat('\z
        color:#FF0000;\z
        background-color:rgba(255,0,0,0.05)')
end

local function setformat_keyword()
    return setformat(nil, true)
end

local function setformat_identifier()
    return setformat()
end

local function setformat_label()
    return setformat('\z
        color:#000080;\z
        font-style:italic')
end

local function setformat_number()
    return setformat('\z
        color:#008080')
end

local function setformat_comment()
    return setformat('\z
        color:#808080;\z
        font-style:italic')
end

local function setformat_longcomment()
    return setformat('\z
        color:#808080;\z
        font-style:italic;\z
        background-color:rgba(128,128,128,0.1)')
end

local function setformat_lightpunctuator()
    return setformat()
end

local function setformat_heavypunctuator()
    return setformat()
end

local function processstring()
    setformat_string()
    local open = getch()
    write(open)
    while true do
        local element = getch()
        if element == '\\' then
            element = element .. getch()
        end
        write(element)
        if element == open or element == '\n' or element == '' then
            break
        end
    end
end

local function processlongstring(level)
    write('[' .. string.rep('=', level) .. '[')
    while true do
        local ch = getch()
        if ch == ']' then
            write(ch)
            for i = 1, level do
                ch = getch()
                if ch == '=' then
                    write(ch)
                else
                    ungetch(ch)
                    ch = nil
                    break
                end
            end
            if ch then
                ch = getch()
                if ch == ']' then
                    write(ch)
                    break
                else
                    ungetch(ch)
                end
            end
        elseif ch == '\n' then
            while peekch() == '\n' do
                getch()
                write('<br>')
            end
            write('\n')
        elseif ch == '' then
            break
        else
            write(ch)
        end
    end
end

local letter = {
    ['_'] = true,
}
local letterordigit = {
    ['_'] = true,
}
local digit = {}
local digit_cont = {
    ['.'] = true,
    ['x'] = true,
    ['X'] = true,
    ['a'] = true,
    ['A'] = true,
    ['b'] = true,
    ['B'] = true,
    ['c'] = true,
    ['C'] = true,
    ['d'] = true,
    ['D'] = true,
    ['e'] = true,
    ['E'] = true,
    ['f'] = true,
    ['F'] = true,
}
for i = string.byte('A'), string.byte('Z') do
    letter[string.char(i)] = true
    letterordigit[string.char(i)] = true
end
for i = string.byte('a'), string.byte('z') do
    letter[string.char(i)] = true
    letterordigit[string.char(i)] = true
end
for i = string.byte('0'), string.byte('9') do
    letterordigit[string.char(i)] = true
    digit[string.char(i)] = true
    digit_cont[string.char(i)] = true
end

local keywords = {}

if kwset == 'lua' then
    for kw in string.gmatch([[
        and       break     do        else      elseif
        end       false     for       function  if
        in        local     nil       not       or
        repeat    return    then      true      until     while
    ]], '[a-z]+') do
        keywords[kw] = true
    end
elseif kwset == 'kon' then
    for kw in string.gmatch([[
        accept
        action
        and
        behavior
        break
        class
        closure
        const
        consume
        current
        default
        do
        else
        elseif
        end
        export
        external
        false
        for
        from
        function
        generic
        goto
        identity
        if
        implementation
        in
        interface
        invoke
        language
        lest
        local
        loop
        modify
        not
        of
        or
        parametric
        produce
        release
        requires
        return
        then
        true
        type
        uses
        while
        with
        yield
    ]], '[a-z]+') do
        keywords[kw] = true
    end
elseif kwset == 'hs' then
    for kw in string.gmatch([[
        case
        data
        do
        else
        forall
        if
        import
        in
        instance
        let
        module
        newtype
        of
        then
        type
        where
    ]], '[a-z]+') do
        keywords[kw] = true
    end
else
    error('wrong kwset')
end

local function processword()
    local word = ''
    while true do
        local ch = getch()
        if letterordigit[ch] then
            word = word .. ch
        else
            ungetch(ch)
            break
        end
    end
    if keywords[word] then
        setformat_keyword()
    else
        setformat_identifier()
    end
    write(word)
end

local function processnumber()
    local number = ''
    while true do
        local ch = getch()
        if ch == 'e' or ch == 'E' then
            number = number .. ch
            ch = peekch()
            if ch == '+' or ch == '-' then
                getch()
                number = numer .. ch
            end
        elseif digit_cont[ch] then
            number = number .. ch
        else
            ungetch(ch)
            break
        end
    end
    setformat_number()
    write(number)
end

local function processlightpunctuator()
    setformat_lightpunctuator()
    write(getch())
end

local function processheavypunctuator()
    setformat_heavypunctuator()
    write(getch())
end

local tokentable = {
    ['\''] = processstring,
    ['"'] = processstring,
    ['.'] = processlightpunctuator,
    ['!'] = processlightpunctuator,
    ['#'] = processlightpunctuator,
    ['+'] = processlightpunctuator,
    ['-'] = processlightpunctuator,
    ['*'] = processlightpunctuator,
    ['/'] = processlightpunctuator,
    ['%'] = processlightpunctuator,
    ['^'] = processlightpunctuator,
    ['<'] = processlightpunctuator,
    ['>'] = processlightpunctuator,
    ['~'] = processlightpunctuator,
    [','] = processlightpunctuator,
    ['='] = processlightpunctuator,
    [']'] = processheavypunctuator,
    ['('] = processheavypunctuator,
    [')'] = processheavypunctuator,
    ['{'] = processheavypunctuator,
    ['}'] = processheavypunctuator,
    [';'] = processheavypunctuator,
}

for ch in pairs(letter) do
    tokentable[ch] = processword
end
for ch in pairs(digit) do
    tokentable[ch] = processnumber
end

tokentable['['] = function()
    getch()
    local level = 0
    while true do
        local ch = peekch()
        if ch == '[' then
            getch()
            setformat_longstring()
            return processlongstring(level)
        elseif ch ~= '=' then
            break
        end
        getch()
        level = level + 1
    end
    setformat_heavypunctuator()
    write('[')
    while level > 0 do
        setformat_lightpunctuator()
        write('=')
        level = level - 1
    end
end

if kwset == 'hs' then
    tokentable[':'] = processlightpunctuator
else
    tokentable[':'] = function()
        getch()
        if peekch() ~= ':' then
            setformat_lightpunctuator()
            write(':')
            return
        end
        getch()
        setformat_label()
        write('::')
        while true do
            local ch = getch()
            if ch == ':' and peekch() == ':' then
                getch()
                write('::')
                break
            elseif ch == '' then
                break
            else
                write(ch)
            end
        end
    end
end

tokentable['-'] = function()
    getch()
    if peekch() ~= '-' then
        setformat_lightpunctuator()
        write('-')
        return
    end
    getch()
    if peekch() == '[' then
        getch()
        local level = 0
        while true do
            local ch = getch()
            if ch == '[' then
                setformat_longcomment()
                write('--')
                return processlongstring(level)
            end
            level = level + 1
            if ch ~= '=' then
                break
            end
        end
        while level > 0 do
            ungetch('=')
            level = level - 1
        end
        ungetch('[')
    end
    setformat_comment()
    write('--')
    while true do
        local ch = getch()
        if ch == '\n' then
            ungetch(ch)
            break
        elseif ch == '' then
            break
        else
            write(ch)
        end
    end
end

tokentable['\n'] = function()
    setformat()
    local ch = getch()
    while peekch() == '\n' do
        getch()
        write('<br>')
    end
    write('\n')
end

tokentable[' '] = function()
    getch()
    write(' ')
end

tokentable['\t'] = function()
    getch()
    write('    ')
end

local function processinput()
    while true do
        local ch = peekch()
        if ch == '' then
            break
        end
        local tfunc = tokentable[ch]
        if tfunc then
            tfunc()
        else
            setformat()
            write(getch())
        end
    end
end

-- write('<html><body>\n')
write('<div style="font-family:monospace;font-size:14px">')
-- write('[quote]')
processinput()
setformat()
-- write('[/quote]')
write('</div>\n')
-- write('</body></html>\n')

-- print(target)

local ffi = require('ffi')

ffi.cdef[[
    int MultiByteToWideChar(
        uint32_t CodePage,
        uint32_t dwFlags,
        char const* lpMultiByteStr,
        int32_t cbMultiByte,
        void* lpWideCharStr,
        int32_t cchWideChar
    );
    bool OpenClipboard(
        void* hWndNewOwner
    );
    bool EmptyClipboard();
    void* SetClipboardData(
        uint32_t uFormat,
        void* hMem
    );
    bool CloseClipboard();
    void* GlobalAlloc(
        uint32_t uFlags,
        size_t dwBytes
    );
    void* GlobalLock(
        void* hMem
    );
    bool GlobalUnlock(
        void* hMem
    );
]]

local function towstr(ustr)
    local buflen = ffi.C.MultiByteToWideChar(65001, 0, ustr, #ustr, nil, 0)
    local buffer = ffi.new('wchar_t[?]', buflen + 1)
    ffi.C.MultiByteToWideChar(65001, 0, ustr, #ustr, buffer, buflen)
    return buffer, buflen + 1
end

local function setclipboard(ustr)
    ffi.C.OpenClipboard(nil)
    ffi.C.EmptyClipboard()
    local buflen = ffi.C.MultiByteToWideChar(65001, 0, ustr, #ustr, nil, 0)
    local handle = ffi.C.GlobalAlloc(2, 2 * (buflen + 1))
    local buffer = ffi.cast('wchar_t*', ffi.C.GlobalLock(handle))
    ffi.C.MultiByteToWideChar(65001, 0, ustr, #ustr, buffer, buflen)
    buffer[buflen] = 0
    ffi.C.GlobalUnlock(buffer)
    ffi.C.SetClipboardData(13, handle)
    ffi.C.CloseClipboard()
end

setclipboard(target)
