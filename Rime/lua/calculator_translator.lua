-- Rime Script >https://github.com/baopaau/rime-lua-collection/blob/master/calculator_translator.lua
-- 簡易計算器（執行任何Lua表達式）
--
-- 格式：=<exp>
-- Lambda語法糖：\<arg>.<exp>|
--
-- 例子：
-- =1+1 輸出 2
-- =floor(9^(8/7)*cos(deg(6))) 輸出 -3
-- =e^pi>pi^e 輸出 true
-- =max({1,7,2}) 輸出 7
-- =map({1,2,3},\x.x^2|) 輸出 {1, 4, 9}
-- =map(range(-5,5),\x.x*pi/4|,deriv(sin)) 輸出 {-0.7071, -1, -0.7071, 0, 0.7071, 1, 0.7071, 0, -0.7071, -1}
-- =$(range(-5,5,0.01))(map,\x.-60*x^2-16*x+20|)(max)() 輸出 21.066
-- =test(\x.trunc(sin(x),1e-3)==trunc(deriv(cos)(x),1e-3)|,range(-2,2,0.1)) 輸出 true
--
-- 安装：
-- - 將本文件保存至 <rime>/lua/
-- - 在 <rime>/rime.lua 新增一行：
--   `calculator_translator = require("calculator_translator")`
-- - 在 <rime>/<schema>.schema.yaml 新增：
--   `engine/translators/@next: lua_translator@calculator_translator`
--   `recognizer/patterns/expression: "^=.*$"`
-- 註：
-- - <rime> 替換爲RIME的共享目錄
-- - <schema> 替換爲自己的方案ID
-- - 如目錄／文件不存在，請自行創建

-- 定義全局函數、常數（注意命名空間污染）
cos = math.cos
sin = math.sin
tan = math.tan
acos = math.acos
asin = math.asin
atan = math.atan
rad = math.rad
deg = math.deg

abs = math.abs
floor = math.floor
ceil = math.ceil
mod = math.fmod
trunc = function (x, dc)
  if dc == nil then
    return math.modf(x)
  end
  return x - mod(x, dc)
end

round = function (x, dc)
  dc = dc or 1
  local dif = mod(x, dc)
  if abs(dif) > dc / 2 then
    return x < 0 and x - dif - dc or x - dif + dc
  end
  return x - dif
end

random = math.random
randomseed = math.randomseed

inf = math.huge
MAX_INT = math.maxinteger
MIN_INT = math.mininteger
pi = math.pi
sqrt = math.sqrt
exp = math.exp
e = exp(1)
ln = math.log
log = function (x, base)
  base = base or 10
  return ln(x)/ln(base)
end

min = function (arr)
  local m = inf
  for k, x in ipairs(arr) do
   m = x < m and x or m
  end
  return m
end

max = function (arr)
  local m = -inf
  for k, x in ipairs(arr) do
   m = x > m and x or m
  end
  return m
end

sum = function (t)
  local acc = 0
  for k,v in ipairs(t) do
    acc = acc + v
  end
  return acc
end

avg = function (t)
  return sum(t) / #t
end

isinteger = function (x)
  return math.fmod(x, 1) == 0
end

-- iterator . array
array = function (...)
  local arr = {}
  for v in ... do
    arr[#arr + 1] = v
  end
  return arr
end

-- iterator <- [form, to)
irange = function (from, to, step)
  if to == nil then
    to = from
    from = 0
  end
  step = step or 1
  local i = from - step
  to = to - step
  return function()
    if i < to then
      i = i + step
      return i
    end
  end
end

-- array <- [form, to)
range = function (from, to, step)
  return array(irange(from, to, step))
end

-- array . reversed iterator
irev = function (arr)
  local i = #arr + 1
  return function()
    if i > 1 then
      i = i - 1
      return arr[i]
    end
  end
end

-- array . reversed array
arev = function (arr)
  return array(irev(arr))
end

test = function (f, t)
  for k,v in ipairs(t) do
    if not f(v) then
      return false
    end
  end
  return true
end

-- # Functional
map = function (t, ...)
  local ta = {}
  for k,v in pairs(t) do
    local tmp = v
    for _,f in pairs({...}) do tmp = f(tmp) end
    ta[k] = tmp
  end
  return ta
end

filter = function (t, ...)
  local ta = {}
  local i = 1
  for k,v in pairs(t) do
    local erase = false
    for _,f in pairs({...}) do
      if not f(v) then
        erase = true
        break
      end
    end
    if not erase then
	  ta[i] = v
	  i = i + 1
    end
  end
  return ta
end

-- e.g: foldr({2,3},\n,x.x^n|,2) = 81
foldr = function (t, f, acc)
  for k,v in pairs(t) do
    acc = f(acc, v)
  end
  return acc
end

-- e.g: foldl({2,3},\n,x.x^n|,2) = 512
foldl = function (t, f, acc)
  for v in irev(t) do
    acc = f(acc, v)
  end
  return acc
end

-- 調用鏈生成函數（HOF for method chaining）
-- e.g: chain(range(-5,5))(map,\x.x/5|)(map,sin)(map,\x.e^x*10|)(map,floor)()
--    = floor(map(map(map(range(-5,5),\x.x/5|),sin),\x.e^x*10|))
--    = {4, 4, 5, 6, 8, 10, 12, 14, 17, 20}
-- 可以用 $ 代替 chain
chain = function (t)
  local ta = t
  local function cf(f, ...)
    if f ~= nil then
      ta = f(ta, ...)
      return cf
    else
      return ta
    end
  end
  return cf
end

-- # Statistics
fac = function (n)
  local acc = 1
  for i = 2,n do
    acc = acc * i
  end
  return acc
end

nPr = function (n, r)
  return fac(n) / fac(n - r)
end

nCr = function (n, r)
  return nPr(n,r) / fac(r)
end

function round45(val, decimal)
  local exp = decimal and 10^decimal or 1
  return math.ceil(val * exp - 0.5) / exp
end

-- 年金现值函数
pa = function(i, n)
  local res = 0
  local nn = 1
  while (nn <= n)
  do
    res = res + 1/(1+i)^nn
    nn = nn + 1
  end
  return round45(res,4)
end

-- 年金终值函数
fa = function(i,n)
  local res = 0
  local nn = n
  while (nn>0)
  do
    nn = nn -1
    res = res + (1+i)^(nn)
  end
  return round45(res,4)
end

-- 现值
pf = function(i,n)
  return round45(1/(1+i)^n,4)
end

MSE = function (t)
  local ss = 0
  local s = 0
  local n = #t
  for k,v in ipairs(t) do
    ss = ss + v*v
    s = s + v
  end
  return sqrt((n*ss - s*s) / (n*n))
end

-- # Linear Algebra


-- # Calculus
-- Linear approximation
lapproxd = function (f, delta)
  local delta = delta or 1e-8
  return function (x)
           return (f(x+delta) - f(x)) / delta
         end
end

-- Symmetric approximation
sapproxd = function (f, delta)
  local delta = delta or 1e-8
  return function (x)
           return (f(x+delta) - f(x-delta)) / delta / 2
         end
end

-- 近似導數
deriv = function (f, delta, dc)
  dc = dc or 1e-4
  local fd = sapproxd(f, delta)
  return function (x)
           return round(fd(x), dc)
         end
end

-- Trapezoidal rule
trapzo = function (f, a, b, n)
  local dif = b - a
  local acc = 0
  for i = 1, n-1 do
    acc = acc + f(a + dif * (i/n))
  end
  acc = acc * 2 + f(a) + f(b)
  acc = acc * dif / n / 2
  return acc
end

-- 近似積分
integ = function (f, delta, dc)
  delta = delta or 1e-4
  dc = dc or 1e-4
  return function (a, b)
           if b == nil then
             b = a
             a = 0
           end
           local n = round(abs(b - a) / delta)
           return round(trapzo(f, a, b, n), dc)
         end
end

-- Runge-Kutta
rk4 = function (f, timestep)
  local timestep = timestep or 0.01
  return function (start_x, start_y, time)
           local x = start_x
           local y = start_y
           local t = time
           -- loop until i >= t
           for i = 0, t, timestep do
             local k1 = f(x, y)
             local k2 = f(x + (timestep/2), y + (timestep/2)*k1)
             local k3 = f(x + (timestep/2), y + (timestep/2)*k2)
             local k4 = f(x + timestep, y + timestep*k3)
             y = y + (timestep/6)*(k1 + 2*k2 + 2*k3 + k4)
             x = x + timestep
           end
           return y
         end
end


-- # System
date = os.date
time = os.time
path = function ()
  return debug.getinfo(1).source:match("@?(.*/)")
end


local function serialize(obj)
  local type = type(obj)
  if type == "number" then
    return isinteger(obj) and floor(obj) or round45(obj,4)
  elseif type == "boolean" then
    return tostring(obj)
  elseif type == "string" then
    return '"'..obj..'"'
  elseif type == "table" then
    local str = "{"
    local i = 1
    for k, v in pairs(obj) do
      if i ~= k then  
        str = str.."["..serialize(k).."]="
      end
      str = str..serialize(v)..", "  
      i = i + 1
    end
    str = str:len() > 3 and str:sub(0,-3) or str
    return str.."}"
  elseif pcall(obj) then -- function類型
    return "callable"
  end
  return obj
end

-- greedy：隨時求值（每次變化都會求值，否則結尾爲特定字符時求值）
local greedy = false

local function splitNumPart(str) 
  local part = {}
  part.int, part.dot, part.dec = string.match(str,"^(%d*)(%.?)(%d*)") 
  return part 
end 

-- 确认精度问题
local function GetPreciseDecimal(nNum, n)
  if type(nNum) ~= "number" then nNum =tonumber(nNum) end 
  n = n or 0;
  n = math.floor(n)
  if n < 0 then n = 0 end
  local nDecimal = 10^n
  local nTemp = math.floor(nNum * nDecimal);
  local nRet = nTemp / nDecimal;
  return nRet;
end

local function decimal_func(str, posMap, valMap) 
  local dec
  posMap = posMap or {[1]="角"; [2]="分"; [3]="厘"; [4]="毫"}
  valMap = valMap or {[0]="零"; "壹"; "贰"; "叁"; "肆"; "伍"; "陆"; "柒"; "捌"; "玖"}
  if #str>4 then dec = string.sub(tostring(str), 1, 4) else dec =tostring(str) end
  dec = string.gsub(dec, "0+$", "")
  if dec == "" then return "整" end
  local result ="" 
  for pos =1, #dec do
      local val = tonumber(string.sub(dec, pos, pos))
      if val~=0 then result = result .. valMap[val] .. posMap[pos] else result = result .. valMap[val] end
  end
  result=result:gsub(valMap[0]..valMap[0] ,valMap[0]) 
  return result:gsub(valMap[0]..valMap[0] ,valMap[0])
end

-- 把数字串按千分位四位数分割，进行转换为中文
local function formatNum(num,t)
  local digitunit,wordFigure
  local result=""
  num=tostring(num)
  if tonumber(t) < 1 then digitunit =	{"", "十", "百", "千"} else digitunit = {"", "拾", "佰", "仟"} end
  if tonumber(t) <1 then
      wordFigure = {"〇", "一", "二", "三", "四", "五", "六", "七", "八", "九"}
  else wordFigure = {"零"; "壹"; "贰"; "叁"; "肆"; "伍"; "陆"; "柒"; "捌"; "玖"} end
  if string.len(num)>4 or tonumber(num)==0 then return wordFigure[1] end
  local lens=string.len(num)
  for i=1,lens do
      local n=wordFigure[tonumber(string.sub(num,-i,-i))+1]
      if n~=wordFigure[1] then result=n .. digitunit[i] .. result else result=n .. result end
  end
  result=result:gsub(wordFigure[1]..wordFigure[1] ,wordFigure[1])
  result=result:gsub(wordFigure[1].."$","") result=result:gsub(wordFigure[1].."$","")
  return result
end

-- 数值转换为中文
function number2cnChar(num,flag,digitunit,wordFigure)	--flag=0 中文小写反之为大写
  local st,result
  num=tostring(num) result=""
  local num1,num2=math.modf(num)
  if tonumber(num2)==0 then
      if tonumber(flag) < 1 then
          digitunit = digitunit or {[1]="万"; [2]="亿"} wordFigure = wordFigure or {[1]="〇"; [2]="一"; [3]="十"; [4]="元"}
      else
          digitunit = digitunit or {[1]="万"; [2]="亿"} wordFigure = wordFigure or {[1]="零"; [2]="壹"; [3]="拾"; [4]="元"}
      end
      local lens=string.len(num1) 
      if lens<5 then result=formatNum(num1,flag) elseif lens<9 then result=formatNum(string.sub(num1,1,-5),flag) .. digitunit[1].. formatNum(string.sub(num1,-4,-1),flag)
      elseif lens<13 then result=formatNum(string.sub(num1,1,-9),flag) .. digitunit[2] .. formatNum(string.sub(num1,-8,-5),flag) .. digitunit[1] .. formatNum(string.sub(num1,-4,-1),flag) else result="" end
      result=result:gsub("^" .. wordFigure[1],"") result=result:gsub(wordFigure[1] .. digitunit[1],"") result=result:gsub(wordFigure[1] .. digitunit[2],"")
      result=result:gsub(wordFigure[1] .. wordFigure[1],wordFigure[1]) result=result:gsub(wordFigure[1] .. "$","")
      if lens>4 then result=result:gsub("^"..wordFigure[2].. wordFigure[3],wordFigure[3]) end
      if result~="" then result=result .. wordFigure[4] else result="数值超限！" end
  else return "数值超限！" end
  return result
end

local function number2zh(num,t)
  local result,wordFigure
  result=""
  if tonumber(t)<1 then
      wordFigure={"〇", "一", "二", "三", "四", "五", "六", "七", "八", "九"}
  else wordFigure={"零"; "壹"; "贰"; "叁"; "肆"; "伍"; "陆"; "柒"; "捌"; "玖"} end
  if tostring(num)==nil then return "" end
  for pos=1,string.len(num) do
      result=result..wordFigure[tonumber(string.sub(num, pos, pos)+1)]
  end
  result=result:gsub(wordFigure[1] .. wordFigure[1], wordFigure[1])
  return result:gsub(wordFigure[1] .. wordFigure[1], wordFigure[1])
end

function number_translatorFunc(num)
  local numberPart=splitNumPart(num)
  local result={}
  if numberPart.dot~="" then
      table.insert(result,{number2cnChar(numberPart.int,0,{"万","亿"},{"〇", "一", "十", "点"})..number2zh(numberPart.dec,0),""})
      table.insert(result,{number2cnChar(numberPart.int,1,{"万","亿"},{"〇", "一", "十", "点"})..number2zh(numberPart.dec,1),""})
      --table.insert(result,{number2cnChar(numberPart.int,0,{"萬","亿"},{"〇", "一", "十", "点"})..number2zh(numberPart.dec,1),""})
      --table.insert(result,{number2cnChar(numberPart.int,0,{"萬","亿"},{"〇", "一", "十", "点"})..number2zh(numberPart.dec,1),"〔大写〕"})
  else
      table.insert(result,{number2cnChar(numberPart.int,0,{"万","亿"},{"〇", "一", "十", "点"}),""})
      table.insert(result,{number2cnChar(numberPart.int,1,{"万","亿"},{"零", "壹", "拾", ""}),""})
  end
  table.insert(result,{number2cnChar(numberPart.int,0)..decimal_func(numberPart.dec,{[1]="角"; [2]="分"; [3]="厘"; [4]="毫"},{[0]="〇"; "一"; "二"; "三"; "四"; "五"; "六"; "七"; "八"; "九"}),""})
  table.insert(result,{number2cnChar(numberPart.int,1)..decimal_func(numberPart.dec,{[1]="角"; [2]="分"; [3]="厘"; [4]="毫"},{[0]="零"; "壹"; "贰"; "叁"; "肄"; "伍"; "陆"; "柒"; "捌"; "玖"}),""})
  return result
end

local function calculator_translator(input, seg)
  if string.sub(input, 1, 1) ~= "=" then return end
  
  if string.sub(input, 2, 2) == "c" then

    local expfin = greedy or string.sub(input, -1, -1) == ";"
    local exp = (greedy or not expfin) and string.sub(input, 3, -1) or string.sub(input, 3, -2)
    if not expfin then return end

    -- 空格輸入可能
    exp = exp:gsub("#", " ")
  
    -- 替换部分符号和标点
    exp = exp:gsub("%*", " * ")
    exp = exp:gsub("%/", " / ")
    exp = exp:gsub("%+", " + ")
    exp = exp:gsub("%-", " - ")

    exp = exp:gsub("%.%.",".")
    exp = exp:gsub("%,%,",",")

    local expe = exp
    -- 鏈式調用語法糖
    expe = expe:gsub("%$", " chain ")
    -- lambda語法糖
    do
      local count
      repeat
        expe, count = expe:gsub("\\%s*([%a%d%s,_]-)%s*%.(.-)|", " (function (%1) return %2 end) ")
      until count == 0
    end
    --yield(Candidate("number", seg.start, seg._end, expe, "展開"))
    
    -- 防止危險操作，禁用os和io命名空間
    if expe:find("i?os?%.") then return end
    -- return語句保證了只有合法的Lua表達式才可執行
    local result = load("return "..expe)()
    if result == nil then return end
    
    result = serialize(result)
    yield(Candidate("number", seg.start, seg._end, exp.." = "..result, ""))
  
  elseif string.sub(input, 2, 2) == "n" then

    local expfin = false or string.sub(input, -1, -1) == ";"
    local exp = (greedy or not expfin) and string.sub(input, 3, -1) or string.sub(input, 3, -2)
    if not expfin then return end

    numberPart=number_translatorFunc(exp:gsub("%.%.","."))
    if #numberPart>0 then
        for i=1, #numberPart do
            yield(Candidate("string", seg.start, seg._end, numberPart[i][1],numberPart[i][2]))
        end
    end
  end
end

return calculator_translator