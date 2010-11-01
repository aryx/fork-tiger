Tiger = Tiger or { }

Tiger.dir = '..' --- where the wild things are

function Tiger.indir(file)
  return Tiger.dir .. '/' .. file -- not portable
end

Ld.libs = Ld.libs .. ' ' .. Tiger.indir('runtime.o') .. ' ' .. Tiger.indir('stdlib.a')

function CMD.compilertab[".tig"](file)
  local tmp = CMD.outfilename(file, '.c--')
  CMD.exec(Tiger.indir('tigerc') .. ' -nogc -cc notail ' .. file .. ' > ' .. tmp)
  return tmp
end

CMD.interptab['.tig'] = CMD.compilertab['.tig']
CMD.prettytab['.tig'] = CMD.compilertab['.tig']

