set keymodel=startsel,stopsel
set ts=4 sw=4

inoremap <C-w> <C-\><C-o>dB
inoremap <C-BS> <C-\><C-o>db

inoremap {<CR> {<CR>}<Esc>ko<tab>
inoremap [<CR> [<CR>]<Esc>ko<tab>
inoremap (<CR> (<CR>)<Esc>ko<tab>

inoremap ><Return> ><Esc>?<[a-z]<CR>lyiwo</<C-r>"><Esc>O<Tab>
inoremap ><Tab> ><Esc>?<[a-z]<CR>lyiwh/[^%]><CR>la</<C-r>"><Esc>F<i
