lower_kaz=['а','ә','б','в','г','ғ','д','е','ё','ж','з','и','й','к','қ','л','м','н','ң','о','ө','п','р','с','т','у','ұ','ү','ф','х','һ','ц','ч','ш','щ','ъ','ы','і','ь','э','ю','я']
upper_kaz=['А','Ә','Б','В','Г','Ғ','Д','Е','Ё','Ж','З','И','Й','К','Қ','Л','М','Н','Ң','О','Ө','П','Р','С','Т','У','Ұ','Ү','Ф','Х','Һ','Ц','Ч','Ш','Щ','Ъ','Ы','І','Ь','Э','Ю','Я']

def print_unicode_as_hex(c):
  val = ord(c)
  lower_bits = val & 0xFF
  s = hex(lower_bits)[1:]
  if s == "x1":
    s = "x01"
  if s == "x5":
    s = "x05"
  if s == "x6":
    s = "x06"
  #print(("  | '\\x04' '\\{}' ".format(s)) + ("{ Lexing.lexeme lexbuf }"))
  return ("'\\x04' '\\{}'".format(s))

def print_unicode_as_hex_2(c):
  b = c.encode('utf-8')
  s = ''.join(f'\\x{byte:02x}' for byte in b)
  first, last = s[:4], s[4:]
  return ("'{}' '{}'".format(first, last))

def as_binary(c):
  v = format(ord(c), '016b')
  return "{} {}".format(v[:8], v[8:])

def print_unicode_for_rule(c):
  print(("  | {} ".format(print_unicode_as_hex)) + ("{ Lexing.lexeme lexbuf }"))

def print_unicode_as_patrn(chars):
  vals = " | ".join([print_unicode_as_hex(x) for x in chars])
  return vals

def print_unicode_as_patrn_2(chars):
  vals = " | ".join([print_unicode_as_hex_2(x) for x in chars])
  return vals

def print_debug(chars):
  vals = "\n".join(["{} - {} - {}".format(print_unicode_as_hex_2(x), as_binary(x), ord(x)) for x in chars])
  return vals

#for c in lower_kaz:
  #print_unicode_as_hex(c)

#for c in upper_kaz:
  #print_unicode_as_hex(c)

#print(print_unicode_as_patrn(lower_kaz))
#print(print_unicode_as_patrn(upper_kaz))

#print(print_debug(lower_kaz))
#print(print_debug(upper_kaz))

print(print_unicode_as_patrn_2(lower_kaz))
print(print_unicode_as_patrn_2(upper_kaz))
