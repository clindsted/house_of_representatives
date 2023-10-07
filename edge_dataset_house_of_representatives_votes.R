library(rvest)
library(dplyr)

get.votes <- function(x){
  id <- runif(1)
  link <- x
  page <- read_html(link)
  table_node <- html_nodes(page,"table") ## list all html tables on the page
  votes <- html_table(table_node)[[2]] ## select table needed
  names(votes) <- c("reps","names","party","state","state_abbrev","vote") ## properly rename columns
  votes <- votes  %>% select(-reps) %>% filter(names != "No data found") %>% mutate(vote = paste0(vote,id)) ## remove duplicate column and make unique table identifier
  return(votes)}

## 118th Congress Votes

## Use get.votes function to scrape data. 

votes_8 <- get.votes('https://clerk.house.gov/Votes/20238?Page=52')
votes_14 <- get.votes('https://clerk.house.gov/Votes/202314?Page=51')
votes_17 <- get.votes('https://clerk.house.gov/Votes/202317?Page=51')
votes_19 <- get.votes('https://clerk.house.gov/Votes/202319?Page=51')
votes_21 <- get.votes('https://clerk.house.gov/Votes/202321?Page=50')
votes_22 <- get.votes('https://clerk.house.gov/Votes/202322?Page=50')
votes_23 <- get.votes('https://clerk.house.gov/Votes/202323?Page=50')
votes_24 <- get.votes('https://clerk.house.gov/Votes/202324?Page=50')
votes_25 <- get.votes('https://clerk.house.gov/Votes/202325?Page=50')
votes_26 <- get.votes('https://clerk.house.gov/Votes/202326?Page=50')
votes_27 <- get.votes('https://clerk.house.gov/Votes/202327?Page=50')
votes_28 <- get.votes('https://clerk.house.gov/Votes/202328?Page=50')
votes_29 <- get.votes('https://clerk.house.gov/Votes/202329?Page=50')
votes_30 <- get.votes('https://clerk.house.gov/Votes/202330?Page=49')
votes_31 <- get.votes('https://clerk.house.gov/Votes/202331?Page=49')
votes_32 <- get.votes('https://clerk.house.gov/Votes/202332?Page=49')
votes_33 <- get.votes('https://clerk.house.gov/Votes/202333?Page=49')
votes_34 <- get.votes('https://clerk.house.gov/Votes/202334?Page=49')
votes_35 <- get.votes('https://clerk.house.gov/Votes/202335?Page=49')
votes_36 <- get.votes('https://clerk.house.gov/Votes/202336?Page=49')
votes_37 <- get.votes('https://clerk.house.gov/Votes/202337?Page=49')
votes_38 <- get.votes('https://clerk.house.gov/Votes/202338?Page=49')
votes_39 <- get.votes('https://clerk.house.gov/Votes/202339?Page=49')
votes_40 <- get.votes('https://clerk.house.gov/Votes/202340?Page=48')
votes_41 <- get.votes('https://clerk.house.gov/Votes/202341?Page=48')
votes_42 <- get.votes('https://clerk.house.gov/Votes/202342?Page=48')
votes_43 <- get.votes('https://clerk.house.gov/Votes/202343?Page=48')
votes_44 <- get.votes('https://clerk.house.gov/Votes/202344?Page=48')
votes_45 <- get.votes('https://clerk.house.gov/Votes/202345?Page=48')
votes_46 <- get.votes('https://clerk.house.gov/Votes/202346?Page=48')
votes_47 <- get.votes('https://clerk.house.gov/Votes/202347?Page=48')
votes_48 <- get.votes('https://clerk.house.gov/Votes/202348?Page=48')
votes_49 <- get.votes('https://clerk.house.gov/Votes/202349?Page=48')
votes_50 <- get.votes('https://clerk.house.gov/Votes/202350?Page=47')
votes_51 <- get.votes('https://clerk.house.gov/Votes/202351?Page=47')
votes_52 <- get.votes('https://clerk.house.gov/Votes/202352?Page=47')
votes_53 <- get.votes('https://clerk.house.gov/Votes/202353?Page=47')
votes_54 <- get.votes('https://clerk.house.gov/Votes/202354?Page=47')
votes_55 <- get.votes('https://clerk.house.gov/Votes/202355?Page=47')
votes_56 <- get.votes('https://clerk.house.gov/Votes/202356?Page=47')
votes_57 <- get.votes('https://clerk.house.gov/Votes/202357?Page=47')
votes_58 <- get.votes('https://clerk.house.gov/Votes/202358?Page=47')
votes_59 <- get.votes('https://clerk.house.gov/Votes/202359?Page=47')
votes_60 <- get.votes('https://clerk.house.gov/Votes/202360?Page=46')
votes_61 <- get.votes('https://clerk.house.gov/Votes/202361?Page=46')
votes_62 <- get.votes('https://clerk.house.gov/Votes/202362?Page=46')
votes_63 <- get.votes('https://clerk.house.gov/Votes/202363?Page=46')
votes_64 <- get.votes('https://clerk.house.gov/Votes/202364?Page=46')
votes_65 <- get.votes('https://clerk.house.gov/Votes/202365?Page=46')
votes_66 <- get.votes('https://clerk.house.gov/Votes/202366?Page=46')
votes_67 <- get.votes('https://clerk.house.gov/Votes/202367?Page=46')
votes_68 <- get.votes('https://clerk.house.gov/Votes/202368?Page=46')
votes_69 <- get.votes('https://clerk.house.gov/Votes/202369?Page=46')
votes_70 <- get.votes('https://clerk.house.gov/Votes/202370?Page=45')
votes_71 <- get.votes('https://clerk.house.gov/Votes/202371?Page=45')
votes_72 <- get.votes('https://clerk.house.gov/Votes/202372?Page=45')
votes_73 <- get.votes('https://clerk.house.gov/Votes/202373?Page=45')
votes_74 <- get.votes('https://clerk.house.gov/Votes/202374?Page=45')
votes_75 <- get.votes('https://clerk.house.gov/Votes/202375?Page=45')
votes_76 <- get.votes('https://clerk.house.gov/Votes/202376?Page=45')
votes_77 <- get.votes('https://clerk.house.gov/Votes/202377?Page=45')
votes_78 <- get.votes('https://clerk.house.gov/Votes/202378?Page=45')
votes_79 <- get.votes('https://clerk.house.gov/Votes/202379?Page=45')
votes_80 <- get.votes('https://clerk.house.gov/Votes/202380?Page=44')
votes_81 <- get.votes('https://clerk.house.gov/Votes/202381?Page=44')
votes_82 <- get.votes('https://clerk.house.gov/Votes/202382?Page=44')
votes_83 <- get.votes('https://clerk.house.gov/Votes/202383?Page=44')
votes_84 <- get.votes('https://clerk.house.gov/Votes/202384?Page=44')
votes_85 <- get.votes('https://clerk.house.gov/Votes/202385?Page=44')
votes_86 <- get.votes('https://clerk.house.gov/Votes/202386?Page=44')
votes_87 <- get.votes('https://clerk.house.gov/Votes/202387?Page=44')
votes_88 <- get.votes('https://clerk.house.gov/Votes/202388?Page=44')
votes_89 <- get.votes('https://clerk.house.gov/Votes/202389?Page=44')
votes_90 <- get.votes('https://clerk.house.gov/Votes/202390?Page=43')
votes_91 <- get.votes('https://clerk.house.gov/Votes/202391?Page=43')
votes_92 <- get.votes('https://clerk.house.gov/Votes/202392?Page=43')
votes_93 <- get.votes('https://clerk.house.gov/Votes/202393?Page=43')
votes_94 <- get.votes('https://clerk.house.gov/Votes/202394?Page=43')
votes_95 <- get.votes('https://clerk.house.gov/Votes/202395?Page=43')
votes_96 <- get.votes('https://clerk.house.gov/Votes/202396?Page=43')
votes_97 <- get.votes('https://clerk.house.gov/Votes/202397?Page=43')
votes_98 <- get.votes('https://clerk.house.gov/Votes/202398?Page=43')
votes_99 <- get.votes('https://clerk.house.gov/Votes/202399?Page=43')
votes_100 <- get.votes('https://clerk.house.gov/Votes/2023100?Page=42')
votes_101 <- get.votes('https://clerk.house.gov/Votes/2023101?Page=42')
votes_102 <- get.votes('https://clerk.house.gov/Votes/2023102?Page=42')
votes_103 <- get.votes('https://clerk.house.gov/Votes/2023103?Page=42')
votes_104 <- get.votes('https://clerk.house.gov/Votes/2023104?Page=42')
votes_105 <- get.votes('https://clerk.house.gov/Votes/2023105?Page=42')
votes_106 <- get.votes('https://clerk.house.gov/Votes/2023106?Page=42')
votes_107 <- get.votes('https://clerk.house.gov/Votes/2023107?Page=42')
votes_108 <- get.votes('https://clerk.house.gov/Votes/2023108?Page=42')
votes_109 <- get.votes('https://clerk.house.gov/Votes/2023109?Page=42')
votes_110 <- get.votes('https://clerk.house.gov/Votes/2023110?Page=41')
votes_111 <- get.votes('https://clerk.house.gov/Votes/2023111?Page=41')
votes_112 <- get.votes('https://clerk.house.gov/Votes/2023112?Page=41')
votes_113 <- get.votes('https://clerk.house.gov/Votes/2023113?Page=41')
votes_114 <- get.votes('https://clerk.house.gov/Votes/2023114?Page=41')
votes_115 <- get.votes('https://clerk.house.gov/Votes/2023115?Page=41')
votes_116 <- get.votes('https://clerk.house.gov/Votes/2023116?Page=41')
votes_117 <- get.votes('https://clerk.house.gov/Votes/2023117?Page=41')
votes_118 <- get.votes('https://clerk.house.gov/Votes/2023118?Page=41')
votes_119 <- get.votes('https://clerk.house.gov/Votes/2023119?Page=41')
votes_120 <- get.votes('https://clerk.house.gov/Votes/2023120?Page=40')
votes_121 <- get.votes('https://clerk.house.gov/Votes/2023121?Page=40')
votes_122 <- get.votes('https://clerk.house.gov/Votes/2023122?Page=40')
votes_123 <- get.votes('https://clerk.house.gov/Votes/2023123?Page=40')
votes_124 <- get.votes('https://clerk.house.gov/Votes/2023124?Page=40')
votes_125 <- get.votes('https://clerk.house.gov/Votes/2023125?Page=40')
votes_126 <- get.votes('https://clerk.house.gov/Votes/2023126?Page=40')
votes_127 <- get.votes('https://clerk.house.gov/Votes/2023127?Page=40')
votes_128 <- get.votes('https://clerk.house.gov/Votes/2023128?Page=40')
votes_129 <- get.votes('https://clerk.house.gov/Votes/2023129?Page=40')
votes_130 <- get.votes('https://clerk.house.gov/Votes/2023130?Page=39')
votes_131 <- get.votes('https://clerk.house.gov/Votes/2023131?Page=39')
votes_132 <- get.votes('https://clerk.house.gov/Votes/2023132?Page=39')
votes_133 <- get.votes('https://clerk.house.gov/Votes/2023133?Page=39')
votes_134 <- get.votes('https://clerk.house.gov/Votes/2023134?Page=39')
votes_135 <- get.votes('https://clerk.house.gov/Votes/2023135?Page=39')
votes_136 <- get.votes('https://clerk.house.gov/Votes/2023136?Page=39')
votes_137 <- get.votes('https://clerk.house.gov/Votes/2023137?Page=39')
votes_138 <- get.votes('https://clerk.house.gov/Votes/2023138?Page=39')
votes_139 <- get.votes('https://clerk.house.gov/Votes/2023139?Page=39')
votes_140 <- get.votes('https://clerk.house.gov/Votes/2023140?Page=38')
votes_141 <- get.votes('https://clerk.house.gov/Votes/2023141?Page=38')
votes_142 <- get.votes('https://clerk.house.gov/Votes/2023142?Page=38')
votes_143 <- get.votes('https://clerk.house.gov/Votes/2023143?Page=38')
votes_144 <- get.votes('https://clerk.house.gov/Votes/2023144?Page=38')
votes_145 <- get.votes('https://clerk.house.gov/Votes/2023145?Page=38')
votes_146 <- get.votes('https://clerk.house.gov/Votes/2023146?Page=38')
votes_147 <- get.votes('https://clerk.house.gov/Votes/2023147?Page=38')
votes_148 <- get.votes('https://clerk.house.gov/Votes/2023148?Page=38')
votes_149 <- get.votes('https://clerk.house.gov/Votes/2023149?Page=38')
votes_150 <- get.votes('https://clerk.house.gov/Votes/2023150?Page=37')
votes_151 <- get.votes('https://clerk.house.gov/Votes/2023151?Page=37')
votes_152 <- get.votes('https://clerk.house.gov/Votes/2023152?Page=37')
votes_153 <- get.votes('https://clerk.house.gov/Votes/2023153?Page=37')
votes_154 <- get.votes('https://clerk.house.gov/Votes/2023154?Page=37')
votes_155 <- get.votes('https://clerk.house.gov/Votes/2023155?Page=37')
votes_156 <- get.votes('https://clerk.house.gov/Votes/2023156?Page=37')
votes_157 <- get.votes('https://clerk.house.gov/Votes/2023157?Page=37')
votes_158 <- get.votes('https://clerk.house.gov/Votes/2023158?Page=37')
votes_159 <- get.votes('https://clerk.house.gov/Votes/2023159?Page=37')
votes_160 <- get.votes('https://clerk.house.gov/Votes/2023160?Page=36')
votes_161 <- get.votes('https://clerk.house.gov/Votes/2023161?Page=36')
votes_162 <- get.votes('https://clerk.house.gov/Votes/2023162?Page=36')
votes_163 <- get.votes('https://clerk.house.gov/Votes/2023163?Page=36')
votes_164 <- get.votes('https://clerk.house.gov/Votes/2023164?Page=36')
votes_165 <- get.votes('https://clerk.house.gov/Votes/2023165?Page=36')
votes_166 <- get.votes('https://clerk.house.gov/Votes/2023166?Page=36')
votes_167 <- get.votes('https://clerk.house.gov/Votes/2023167?Page=36')
votes_168 <- get.votes('https://clerk.house.gov/Votes/2023168?Page=36')
votes_169 <- get.votes('https://clerk.house.gov/Votes/2023169?Page=36')
votes_170 <- get.votes('https://clerk.house.gov/Votes/2023170?Page=35')
votes_171 <- get.votes('https://clerk.house.gov/Votes/2023171?Page=35')
votes_172 <- get.votes('https://clerk.house.gov/Votes/2023172?Page=35')
votes_173 <- get.votes('https://clerk.house.gov/Votes/2023173?Page=35')
votes_174 <- get.votes('https://clerk.house.gov/Votes/2023174?Page=35')
votes_175 <- get.votes('https://clerk.house.gov/Votes/2023175?Page=35')
votes_176 <- get.votes('https://clerk.house.gov/Votes/2023176?Page=35')
votes_177 <- get.votes('https://clerk.house.gov/Votes/2023177?Page=35')
votes_178 <- get.votes('https://clerk.house.gov/Votes/2023178?Page=35')
votes_179 <- get.votes('https://clerk.house.gov/Votes/2023179?Page=35')
votes_180 <- get.votes('https://clerk.house.gov/Votes/2023180?Page=34')
votes_181 <- get.votes('https://clerk.house.gov/Votes/2023181?Page=34')
votes_182 <- get.votes('https://clerk.house.gov/Votes/2023182?Page=34')
votes_183 <- get.votes('https://clerk.house.gov/Votes/2023183?Page=34')
votes_184 <- get.votes('https://clerk.house.gov/Votes/2023184?Page=34')
votes_185 <- get.votes('https://clerk.house.gov/Votes/2023185?Page=34')
votes_186 <- get.votes('https://clerk.house.gov/Votes/2023186?Page=34')
votes_187 <- get.votes('https://clerk.house.gov/Votes/2023187?Page=34')
votes_188 <- get.votes('https://clerk.house.gov/Votes/2023188?Page=34')
votes_189 <- get.votes('https://clerk.house.gov/Votes/2023189?Page=34')
votes_190 <- get.votes('https://clerk.house.gov/Votes/2023190?Page=33')
votes_191 <- get.votes('https://clerk.house.gov/Votes/2023191?Page=33')
votes_192 <- get.votes('https://clerk.house.gov/Votes/2023192?Page=33')
votes_193 <- get.votes('https://clerk.house.gov/Votes/2023193?Page=33')
votes_194 <- get.votes('https://clerk.house.gov/Votes/2023194?Page=33')
votes_195 <- get.votes('https://clerk.house.gov/Votes/2023195?Page=33')
votes_196 <- get.votes('https://clerk.house.gov/Votes/2023196?Page=33')
votes_197 <- get.votes('https://clerk.house.gov/Votes/2023197?Page=33')
votes_198 <- get.votes('https://clerk.house.gov/Votes/2023198?Page=33')
votes_199 <- get.votes('https://clerk.house.gov/Votes/2023199?Page=33')
votes_200 <- get.votes('https://clerk.house.gov/Votes/2023200?Page=32')
votes_201 <- get.votes('https://clerk.house.gov/Votes/2023201?Page=32')
votes_202 <- get.votes('https://clerk.house.gov/Votes/2023202?Page=32')
votes_203 <- get.votes('https://clerk.house.gov/Votes/2023203?Page=32')
votes_204 <- get.votes('https://clerk.house.gov/Votes/2023204?Page=32')
votes_205 <- get.votes('https://clerk.house.gov/Votes/2023205?Page=32')
votes_206 <- get.votes('https://clerk.house.gov/Votes/2023206?Page=32')
votes_207 <- get.votes('https://clerk.house.gov/Votes/2023207?Page=32')
votes_208 <- get.votes('https://clerk.house.gov/Votes/2023208?Page=32')
votes_209 <- get.votes('https://clerk.house.gov/Votes/2023209?Page=32')
votes_210 <- get.votes('https://clerk.house.gov/Votes/2023210?Page=31')
votes_211 <- get.votes('https://clerk.house.gov/Votes/2023211?Page=31')
votes_212 <- get.votes('https://clerk.house.gov/Votes/2023212?Page=31')
votes_213 <- get.votes('https://clerk.house.gov/Votes/2023213?Page=31')
votes_214 <- get.votes('https://clerk.house.gov/Votes/2023214?Page=31')
votes_215 <- get.votes('https://clerk.house.gov/Votes/2023215?Page=31')
votes_216 <- get.votes('https://clerk.house.gov/Votes/2023216?Page=31')
votes_217 <- get.votes('https://clerk.house.gov/Votes/2023217?Page=31')
votes_218 <- get.votes('https://clerk.house.gov/Votes/2023218?Page=31')
votes_219 <- get.votes('https://clerk.house.gov/Votes/2023219?Page=31')
votes_220 <- get.votes('https://clerk.house.gov/Votes/2023220?Page=30')
votes_221 <- get.votes('https://clerk.house.gov/Votes/2023221?Page=30')
votes_222 <- get.votes('https://clerk.house.gov/Votes/2023222?Page=30')
votes_223 <- get.votes('https://clerk.house.gov/Votes/2023223?Page=30')
votes_224 <- get.votes('https://clerk.house.gov/Votes/2023224?Page=30')
votes_225 <- get.votes('https://clerk.house.gov/Votes/2023225?Page=30')
votes_226 <- get.votes('https://clerk.house.gov/Votes/2023226?Page=30')
votes_227 <- get.votes('https://clerk.house.gov/Votes/2023227?Page=30')
votes_228 <- get.votes('https://clerk.house.gov/Votes/2023228?Page=30')
votes_229 <- get.votes('https://clerk.house.gov/Votes/2023229?Page=30')
votes_230 <- get.votes('https://clerk.house.gov/Votes/2023230?Page=29')
votes_231 <- get.votes('https://clerk.house.gov/Votes/2023231?Page=29')
votes_232 <- get.votes('https://clerk.house.gov/Votes/2023232?Page=29')
votes_233 <- get.votes('https://clerk.house.gov/Votes/2023233?Page=29')
votes_234 <- get.votes('https://clerk.house.gov/Votes/2023234?Page=29')
votes_235 <- get.votes('https://clerk.house.gov/Votes/2023235?Page=29')
votes_236 <- get.votes('https://clerk.house.gov/Votes/2023236?Page=29')
votes_237 <- get.votes('https://clerk.house.gov/Votes/2023237?Page=29')
votes_238 <- get.votes('https://clerk.house.gov/Votes/2023238?Page=29')
votes_239 <- get.votes('https://clerk.house.gov/Votes/2023239?Page=29')
votes_240 <- get.votes('https://clerk.house.gov/Votes/2023240?Page=28')
votes_241 <- get.votes('https://clerk.house.gov/Votes/2023241?Page=28')
votes_242 <- get.votes('https://clerk.house.gov/Votes/2023242?Page=28')
votes_243 <- get.votes('https://clerk.house.gov/Votes/2023243?Page=28')
votes_244 <- get.votes('https://clerk.house.gov/Votes/2023244?Page=28')
votes_245 <- get.votes('https://clerk.house.gov/Votes/2023245?Page=28')
votes_246 <- get.votes('https://clerk.house.gov/Votes/2023246?Page=28')
votes_247 <- get.votes('https://clerk.house.gov/Votes/2023247?Page=28')
votes_248 <- get.votes('https://clerk.house.gov/Votes/2023248?Page=28')
votes_249 <- get.votes('https://clerk.house.gov/Votes/2023249?Page=28')
votes_250 <- get.votes('https://clerk.house.gov/Votes/2023250?Page=27')
votes_251 <- get.votes('https://clerk.house.gov/Votes/2023251?Page=27')
votes_252 <- get.votes('https://clerk.house.gov/Votes/2023252?Page=27')
votes_253 <- get.votes('https://clerk.house.gov/Votes/2023253?Page=27')
votes_254 <- get.votes('https://clerk.house.gov/Votes/2023254?Page=27')
votes_255 <- get.votes('https://clerk.house.gov/Votes/2023255?Page=27')
votes_256 <- get.votes('https://clerk.house.gov/Votes/2023256?Page=27')
votes_257 <- get.votes('https://clerk.house.gov/Votes/2023257?Page=27')
votes_258 <- get.votes('https://clerk.house.gov/Votes/2023258?Page=27')
votes_259 <- get.votes('https://clerk.house.gov/Votes/2023259?Page=27')
votes_260 <- get.votes('https://clerk.house.gov/Votes/2023260?Page=26')
votes_261 <- get.votes('https://clerk.house.gov/Votes/2023261?Page=26')
votes_262 <- get.votes('https://clerk.house.gov/Votes/2023262?Page=26')
votes_263 <- get.votes('https://clerk.house.gov/Votes/2023263?Page=26')
votes_264 <- get.votes('https://clerk.house.gov/Votes/2023264?Page=26')
votes_265 <- get.votes('https://clerk.house.gov/Votes/2023265?Page=26')
votes_266 <- get.votes('https://clerk.house.gov/Votes/2023266?Page=26')
votes_267 <- get.votes('https://clerk.house.gov/Votes/2023267?Page=26')
votes_268 <- get.votes('https://clerk.house.gov/Votes/2023268?Page=26')
votes_269 <- get.votes('https://clerk.house.gov/Votes/2023269?Page=26')
votes_270 <- get.votes('https://clerk.house.gov/Votes/2023270?Page=25')
votes_271 <- get.votes('https://clerk.house.gov/Votes/2023271?Page=25')
votes_272 <- get.votes('https://clerk.house.gov/Votes/2023272?Page=25')
votes_273 <- get.votes('https://clerk.house.gov/Votes/2023273?Page=25')
votes_274 <- get.votes('https://clerk.house.gov/Votes/2023274?Page=25')
votes_275 <- get.votes('https://clerk.house.gov/Votes/2023275?Page=25')
votes_276 <- get.votes('https://clerk.house.gov/Votes/2023276?Page=25')
votes_277 <- get.votes('https://clerk.house.gov/Votes/2023277?Page=25')
votes_278 <- get.votes('https://clerk.house.gov/Votes/2023278?Page=25')
votes_279 <- get.votes('https://clerk.house.gov/Votes/2023279?Page=25')
votes_280 <- get.votes('https://clerk.house.gov/Votes/2023280?Page=24')
votes_281 <- get.votes('https://clerk.house.gov/Votes/2023281?Page=24')
votes_282 <- get.votes('https://clerk.house.gov/Votes/2023282?Page=24')
votes_283 <- get.votes('https://clerk.house.gov/Votes/2023283?Page=24')
votes_284 <- get.votes('https://clerk.house.gov/Votes/2023284?Page=24')
votes_285 <- get.votes('https://clerk.house.gov/Votes/2023285?Page=24')
votes_286 <- get.votes('https://clerk.house.gov/Votes/2023286?Page=24')
votes_287 <- get.votes('https://clerk.house.gov/Votes/2023287?Page=24')
votes_288 <- get.votes('https://clerk.house.gov/Votes/2023288?Page=24')
votes_289 <- get.votes('https://clerk.house.gov/Votes/2023289?Page=24')
votes_290 <- get.votes('https://clerk.house.gov/Votes/2023290?Page=23')
votes_291 <- get.votes('https://clerk.house.gov/Votes/2023291?Page=23')
votes_292 <- get.votes('https://clerk.house.gov/Votes/2023292?Page=23')
votes_293 <- get.votes('https://clerk.house.gov/Votes/2023293?Page=23')
votes_294 <- get.votes('https://clerk.house.gov/Votes/2023294?Page=23')
votes_295 <- get.votes('https://clerk.house.gov/Votes/2023295?Page=23')
votes_296 <- get.votes('https://clerk.house.gov/Votes/2023296?Page=23')
votes_297 <- get.votes('https://clerk.house.gov/Votes/2023297?Page=23')
votes_298 <- get.votes('https://clerk.house.gov/Votes/2023298?Page=23')
votes_299 <- get.votes('https://clerk.house.gov/Votes/2023299?Page=23')
votes_300 <- get.votes('https://clerk.house.gov/Votes/2023300?Page=22')
votes_301 <- get.votes('https://clerk.house.gov/Votes/2023301?Page=22')
votes_302 <- get.votes('https://clerk.house.gov/Votes/2023302?Page=22')
votes_303 <- get.votes('https://clerk.house.gov/Votes/2023303?Page=22')
votes_304 <- get.votes('https://clerk.house.gov/Votes/2023304?Page=22')
votes_305 <- get.votes('https://clerk.house.gov/Votes/2023305?Page=22')
votes_306 <- get.votes('https://clerk.house.gov/Votes/2023306?Page=22')
votes_307 <- get.votes('https://clerk.house.gov/Votes/2023307?Page=22')
votes_308 <- get.votes('https://clerk.house.gov/Votes/2023308?Page=22')
votes_309 <- get.votes('https://clerk.house.gov/Votes/2023309?Page=22')
votes_310 <- get.votes('https://clerk.house.gov/Votes/2023310?Page=21')
votes_311 <- get.votes('https://clerk.house.gov/Votes/2023311?Page=21')
votes_312 <- get.votes('https://clerk.house.gov/Votes/2023312?Page=21')
votes_313 <- get.votes('https://clerk.house.gov/Votes/2023313?Page=21')
votes_314 <- get.votes('https://clerk.house.gov/Votes/2023314?Page=21')
votes_315 <- get.votes('https://clerk.house.gov/Votes/2023315?Page=21')
votes_316 <- get.votes('https://clerk.house.gov/Votes/2023316?Page=21')
votes_317 <- get.votes('https://clerk.house.gov/Votes/2023317?Page=21')
votes_318 <- get.votes('https://clerk.house.gov/Votes/2023318?Page=21')
votes_319 <- get.votes('https://clerk.house.gov/Votes/2023319?Page=21')
votes_320 <- get.votes('https://clerk.house.gov/Votes/2023320?Page=20')
votes_321 <- get.votes('https://clerk.house.gov/Votes/2023321?Page=20')
votes_322 <- get.votes('https://clerk.house.gov/Votes/2023322?Page=20')
votes_323 <- get.votes('https://clerk.house.gov/Votes/2023323?Page=20')
votes_324 <- get.votes('https://clerk.house.gov/Votes/2023324?Page=20')
votes_325 <- get.votes('https://clerk.house.gov/Votes/2023325?Page=20')
votes_326 <- get.votes('https://clerk.house.gov/Votes/2023326?Page=20')
votes_327 <- get.votes('https://clerk.house.gov/Votes/2023327?Page=20')
votes_328 <- get.votes('https://clerk.house.gov/Votes/2023328?Page=20')
votes_329 <- get.votes('https://clerk.house.gov/Votes/2023329?Page=20')
votes_330 <- get.votes('https://clerk.house.gov/Votes/2023330?Page=19')
votes_331 <- get.votes('https://clerk.house.gov/Votes/2023331?Page=19')
votes_332 <- get.votes('https://clerk.house.gov/Votes/2023332?Page=19')
votes_333 <- get.votes('https://clerk.house.gov/Votes/2023333?Page=19')
votes_334 <- get.votes('https://clerk.house.gov/Votes/2023334?Page=19')
votes_335 <- get.votes('https://clerk.house.gov/Votes/2023335?Page=19')
votes_336 <- get.votes('https://clerk.house.gov/Votes/2023336?Page=19')
votes_337 <- get.votes('https://clerk.house.gov/Votes/2023337?Page=19')
votes_338 <- get.votes('https://clerk.house.gov/Votes/2023338?Page=19')
votes_339 <- get.votes('https://clerk.house.gov/Votes/2023339?Page=19')
votes_340 <- get.votes('https://clerk.house.gov/Votes/2023340?Page=18')
votes_341 <- get.votes('https://clerk.house.gov/Votes/2023341?Page=18')
votes_342 <- get.votes('https://clerk.house.gov/Votes/2023342?Page=18')
votes_343 <- get.votes('https://clerk.house.gov/Votes/2023343?Page=18')
votes_344 <- get.votes('https://clerk.house.gov/Votes/2023344?Page=18')
votes_345 <- get.votes('https://clerk.house.gov/Votes/2023345?Page=18')
votes_346 <- get.votes('https://clerk.house.gov/Votes/2023346?Page=18')
votes_347 <- get.votes('https://clerk.house.gov/Votes/2023347?Page=18')
votes_348 <- get.votes('https://clerk.house.gov/Votes/2023348?Page=18')
votes_349 <- get.votes('https://clerk.house.gov/Votes/2023349?Page=18')
votes_350 <- get.votes('https://clerk.house.gov/Votes/2023350?Page=17')
votes_351 <- get.votes('https://clerk.house.gov/Votes/2023351?Page=17')
votes_352 <- get.votes('https://clerk.house.gov/Votes/2023352?Page=17')
votes_353 <- get.votes('https://clerk.house.gov/Votes/2023353?Page=17')
votes_354 <- get.votes('https://clerk.house.gov/Votes/2023354?Page=17')
votes_355 <- get.votes('https://clerk.house.gov/Votes/2023355?Page=17')
votes_356 <- get.votes('https://clerk.house.gov/Votes/2023356?Page=17')
votes_357 <- get.votes('https://clerk.house.gov/Votes/2023357?Page=17')
votes_358 <- get.votes('https://clerk.house.gov/Votes/2023358?Page=17')
votes_359 <- get.votes('https://clerk.house.gov/Votes/2023359?Page=17')
votes_360 <- get.votes('https://clerk.house.gov/Votes/2023360?Page=16')
votes_361 <- get.votes('https://clerk.house.gov/Votes/2023361?Page=16')
votes_362 <- get.votes('https://clerk.house.gov/Votes/2023362?Page=16')
votes_363 <- get.votes('https://clerk.house.gov/Votes/2023363?Page=16')
votes_364 <- get.votes('https://clerk.house.gov/Votes/2023364?Page=16')
votes_365 <- get.votes('https://clerk.house.gov/Votes/2023365?Page=16')
votes_366 <- get.votes('https://clerk.house.gov/Votes/2023366?Page=16')
votes_367 <- get.votes('https://clerk.house.gov/Votes/2023367?Page=16')
votes_368 <- get.votes('https://clerk.house.gov/Votes/2023368?Page=16')
votes_369 <- get.votes('https://clerk.house.gov/Votes/2023369?Page=16')
votes_370 <- get.votes('https://clerk.house.gov/Votes/2023370?Page=15')
votes_371 <- get.votes('https://clerk.house.gov/Votes/2023371?Page=15')
votes_372 <- get.votes('https://clerk.house.gov/Votes/2023372?Page=15')
votes_373 <- get.votes('https://clerk.house.gov/Votes/2023373?Page=15')
votes_374 <- get.votes('https://clerk.house.gov/Votes/2023374?Page=15')
votes_375 <- get.votes('https://clerk.house.gov/Votes/2023375?Page=15')
votes_376 <- get.votes('https://clerk.house.gov/Votes/2023376?Page=15')
votes_377 <- get.votes('https://clerk.house.gov/Votes/2023377?Page=15')
votes_378 <- get.votes('https://clerk.house.gov/Votes/2023378?Page=15')
votes_379 <- get.votes('https://clerk.house.gov/Votes/2023379?Page=15')
votes_380 <- get.votes('https://clerk.house.gov/Votes/2023380?Page=14')
votes_381 <- get.votes('https://clerk.house.gov/Votes/2023381?Page=14')
votes_382 <- get.votes('https://clerk.house.gov/Votes/2023382?Page=14')
votes_383 <- get.votes('https://clerk.house.gov/Votes/2023383?Page=14')
votes_384 <- get.votes('https://clerk.house.gov/Votes/2023384?Page=14')
votes_385 <- get.votes('https://clerk.house.gov/Votes/2023385?Page=14')
votes_386 <- get.votes('https://clerk.house.gov/Votes/2023386?Page=14')
votes_387 <- get.votes('https://clerk.house.gov/Votes/2023387?Page=14')
votes_388 <- get.votes('https://clerk.house.gov/Votes/2023388?Page=14')
votes_389 <- get.votes('https://clerk.house.gov/Votes/2023389?Page=14')
votes_390 <- get.votes('https://clerk.house.gov/Votes/2023390?Page=13')
votes_391 <- get.votes('https://clerk.house.gov/Votes/2023391?Page=13')
votes_392 <- get.votes('https://clerk.house.gov/Votes/2023392?Page=13')
votes_393 <- get.votes('https://clerk.house.gov/Votes/2023393?Page=13')
votes_394 <- get.votes('https://clerk.house.gov/Votes/2023394?Page=13')
votes_395 <- get.votes('https://clerk.house.gov/Votes/2023395?Page=13')
votes_396 <- get.votes('https://clerk.house.gov/Votes/2023396?Page=13')
votes_397 <- get.votes('https://clerk.house.gov/Votes/2023397?Page=13')
votes_398 <- get.votes('https://clerk.house.gov/Votes/2023398?Page=13')
votes_399 <- get.votes('https://clerk.house.gov/Votes/2023399?Page=13')
votes_400 <- get.votes('https://clerk.house.gov/Votes/2023400?Page=12')
votes_401 <- get.votes('https://clerk.house.gov/Votes/2023401?Page=12')
votes_402 <- get.votes('https://clerk.house.gov/Votes/2023402?Page=12')
votes_403 <- get.votes('https://clerk.house.gov/Votes/2023403?Page=12')
votes_404 <- get.votes('https://clerk.house.gov/Votes/2023404?Page=12')
votes_405 <- get.votes('https://clerk.house.gov/Votes/2023405?Page=12')
votes_406 <- get.votes('https://clerk.house.gov/Votes/2023406?Page=12')
votes_407 <- get.votes('https://clerk.house.gov/Votes/2023407?Page=12')
votes_408 <- get.votes('https://clerk.house.gov/Votes/2023408?Page=12')
votes_409 <- get.votes('https://clerk.house.gov/Votes/2023409?Page=12')
votes_410 <- get.votes('https://clerk.house.gov/Votes/2023410?Page=11')
votes_411 <- get.votes('https://clerk.house.gov/Votes/2023411?Page=11')
votes_412 <- get.votes('https://clerk.house.gov/Votes/2023412?Page=11')
votes_413 <- get.votes('https://clerk.house.gov/Votes/2023413?Page=11')
votes_414 <- get.votes('https://clerk.house.gov/Votes/2023414?Page=11')
votes_415 <- get.votes('https://clerk.house.gov/Votes/2023415?Page=11')
votes_416 <- get.votes('https://clerk.house.gov/Votes/2023416?Page=11')
votes_417 <- get.votes('https://clerk.house.gov/Votes/2023417?Page=11')
votes_418 <- get.votes('https://clerk.house.gov/Votes/2023418?Page=11')
votes_419 <- get.votes('https://clerk.house.gov/Votes/2023419?Page=11')
votes_420 <- get.votes('https://clerk.house.gov/Votes/2023420?Page=10')
votes_421 <- get.votes('https://clerk.house.gov/Votes/2023421?Page=10')
votes_422 <- get.votes('https://clerk.house.gov/Votes/2023422?Page=10')
votes_423 <- get.votes('https://clerk.house.gov/Votes/2023423?Page=10')
votes_424 <- get.votes('https://clerk.house.gov/Votes/2023424?Page=10')
votes_425 <- get.votes('https://clerk.house.gov/Votes/2023425?Page=10')
votes_426 <- get.votes('https://clerk.house.gov/Votes/2023426?Page=10')
votes_427 <- get.votes('https://clerk.house.gov/Votes/2023427?Page=10')
votes_428 <- get.votes('https://clerk.house.gov/Votes/2023428?Page=10')
votes_429 <- get.votes('https://clerk.house.gov/Votes/2023429?Page=10')
votes_430 <- get.votes('https://clerk.house.gov/Votes/2023430?Page=9')
votes_431 <- get.votes('https://clerk.house.gov/Votes/2023431?Page=9')
votes_432 <- get.votes('https://clerk.house.gov/Votes/2023432?Page=9')
votes_433 <- get.votes('https://clerk.house.gov/Votes/2023433?Page=9')
votes_434 <- get.votes('https://clerk.house.gov/Votes/2023434?Page=9')
votes_435 <- get.votes('https://clerk.house.gov/Votes/2023435?Page=9')
votes_436 <- get.votes('https://clerk.house.gov/Votes/2023436?Page=9')
votes_437 <- get.votes('https://clerk.house.gov/Votes/2023437?Page=9')
votes_438 <- get.votes('https://clerk.house.gov/Votes/2023438?Page=9')
votes_439 <- get.votes('https://clerk.house.gov/Votes/2023439?Page=9')
votes_440 <- get.votes('https://clerk.house.gov/Votes/2023440?Page=8')
votes_441 <- get.votes('https://clerk.house.gov/Votes/2023441?Page=8')
votes_442 <- get.votes('https://clerk.house.gov/Votes/2023442?Page=8')
votes_443 <- get.votes('https://clerk.house.gov/Votes/2023443?Page=8')
votes_444 <- get.votes('https://clerk.house.gov/Votes/2023444?Page=8')
votes_445 <- get.votes('https://clerk.house.gov/Votes/2023445?Page=8')
votes_446 <- get.votes('https://clerk.house.gov/Votes/2023446?Page=8')
votes_447 <- get.votes('https://clerk.house.gov/Votes/2023447?Page=8')
votes_448 <- get.votes('https://clerk.house.gov/Votes/2023448?Page=8')
votes_449 <- get.votes('https://clerk.house.gov/Votes/2023449?Page=8')
votes_450 <- get.votes('https://clerk.house.gov/Votes/2023450?Page=7')
votes_451 <- get.votes('https://clerk.house.gov/Votes/2023451?Page=7')
votes_452 <- get.votes('https://clerk.house.gov/Votes/2023452?Page=7')
votes_453 <- get.votes('https://clerk.house.gov/Votes/2023453?Page=7')
votes_454 <- get.votes('https://clerk.house.gov/Votes/2023454?Page=7')
votes_455 <- get.votes('https://clerk.house.gov/Votes/2023455?Page=7')
votes_456 <- get.votes('https://clerk.house.gov/Votes/2023456?Page=7')
votes_457 <- get.votes('https://clerk.house.gov/Votes/2023457?Page=7')
votes_458 <- get.votes('https://clerk.house.gov/Votes/2023458?Page=7')
votes_459 <- get.votes('https://clerk.house.gov/Votes/2023459?Page=7')
votes_460 <- get.votes('https://clerk.house.gov/Votes/2023460?Page=6')
votes_461 <- get.votes('https://clerk.house.gov/Votes/2023461?Page=6')
votes_462 <- get.votes('https://clerk.house.gov/Votes/2023462?Page=6')
votes_463 <- get.votes('https://clerk.house.gov/Votes/2023463?Page=6')
votes_464 <- get.votes('https://clerk.house.gov/Votes/2023464?Page=6')
votes_465 <- get.votes('https://clerk.house.gov/Votes/2023465?Page=6')
votes_466 <- get.votes('https://clerk.house.gov/Votes/2023466?Page=6')
votes_467 <- get.votes('https://clerk.house.gov/Votes/2023467?Page=6')
votes_468 <- get.votes('https://clerk.house.gov/Votes/2023468?Page=6')
votes_469 <- get.votes('https://clerk.house.gov/Votes/2023469?Page=6')
votes_470 <- get.votes('https://clerk.house.gov/Votes/2023470?Page=5')
votes_471 <- get.votes('https://clerk.house.gov/Votes/2023471?Page=5')
votes_472 <- get.votes('https://clerk.house.gov/Votes/2023472?Page=5')
votes_473 <- get.votes('https://clerk.house.gov/Votes/2023473?Page=5')
votes_474 <- get.votes('https://clerk.house.gov/Votes/2023474?Page=5')
votes_475 <- get.votes('https://clerk.house.gov/Votes/2023475?Page=5')
votes_476 <- get.votes('https://clerk.house.gov/Votes/2023476?Page=5')
votes_477 <- get.votes('https://clerk.house.gov/Votes/2023477?Page=5')
votes_478 <- get.votes('https://clerk.house.gov/Votes/2023478?Page=5')
votes_479 <- get.votes('https://clerk.house.gov/Votes/2023479?Page=5')
votes_480 <- get.votes('https://clerk.house.gov/Votes/2023480?Page=4')
votes_481 <- get.votes('https://clerk.house.gov/Votes/2023481?Page=4')
votes_482 <- get.votes('https://clerk.house.gov/Votes/2023482?Page=4')
votes_483 <- get.votes('https://clerk.house.gov/Votes/2023483?Page=4')
votes_484 <- get.votes('https://clerk.house.gov/Votes/2023484?Page=4')
votes_485 <- get.votes('https://clerk.house.gov/Votes/2023485?Page=4')
votes_486 <- get.votes('https://clerk.house.gov/Votes/2023486?Page=4')
votes_487 <- get.votes('https://clerk.house.gov/Votes/2023487?Page=4')
votes_488 <- get.votes('https://clerk.house.gov/Votes/2023488?Page=4')
votes_489 <- get.votes('https://clerk.house.gov/Votes/2023489?Page=4')
votes_490 <- get.votes('https://clerk.house.gov/Votes/2023490?Page=3')
votes_491 <- get.votes('https://clerk.house.gov/Votes/2023491?Page=3')
votes_492 <- get.votes('https://clerk.house.gov/Votes/2023492?Page=3')
votes_493 <- get.votes('https://clerk.house.gov/Votes/2023493?Page=3')
votes_494 <- get.votes('https://clerk.house.gov/Votes/2023494?Page=3')
votes_495 <- get.votes('https://clerk.house.gov/Votes/2023495?Page=3')
votes_496 <- get.votes('https://clerk.house.gov/Votes/2023496?Page=3')
votes_497 <- get.votes('https://clerk.house.gov/Votes/2023497?Page=3')
votes_498 <- get.votes('https://clerk.house.gov/Votes/2023498?Page=3')
votes_499 <- get.votes('https://clerk.house.gov/Votes/2023499?Page=3')
votes_500 <- get.votes('https://clerk.house.gov/Votes/2023500?Page=2')
votes_501 <- get.votes('https://clerk.house.gov/Votes/2023501?Page=2')
votes_502 <- get.votes('https://clerk.house.gov/Votes/2023502?Page=2')
votes_503 <- get.votes('https://clerk.house.gov/Votes/2023503?Page=2')
votes_504 <- get.votes('https://clerk.house.gov/Votes/2023504?Page=2')
votes_505 <- get.votes('https://clerk.house.gov/Votes/2023505?Page=2')
votes_506 <- get.votes('https://clerk.house.gov/Votes/2023506?Page=2')
votes_507 <- get.votes('https://clerk.house.gov/Votes/2023507?Page=2')
votes_508 <- get.votes('https://clerk.house.gov/Votes/2023508?Page=2')
votes_509 <- get.votes('https://clerk.house.gov/Votes/2023509?Page=2')
votes_510 <- get.votes('https://clerk.house.gov/Votes/2023510?Page=1')
votes_511 <- get.votes('https://clerk.house.gov/Votes/2023511?Page=1')
votes_512 <- get.votes('https://clerk.house.gov/Votes/2023512?Page=1')
votes_513 <- get.votes('https://clerk.house.gov/Votes/2023513?Page=1')
votes_514 <- get.votes('https://clerk.house.gov/Votes/2023514?Page=1')
votes_515 <- get.votes('https://clerk.house.gov/Votes/2023515?Page=1')
votes_516 <- get.votes('https://clerk.house.gov/Votes/2023516?Page=1')
votes_517 <- get.votes('https://clerk.house.gov/Votes/2023517?Page=1')
votes_518 <- get.votes('https://clerk.house.gov/Votes/2023518?Page=1')
votes_519 <- get.votes('https://clerk.house.gov/Votes/2023519?Page=1')

## Merge sets

total_votes <- rbind(
                      votes_8,
                      votes_14,
                      votes_17,
                      votes_19,
                      votes_21,
                      votes_22,
                      votes_23,
                      votes_24,
                      votes_25,
                      votes_26,
                      votes_27,
                      votes_28,
                      votes_29,
                      votes_30,
                      votes_31,
                      votes_32,
                      votes_33,
                      votes_34,
                      votes_35,
                      votes_36,
                      votes_37,
                      votes_38,
                      votes_39,
                      votes_40,
                      votes_41,
                      votes_42,
                      votes_43,
                      votes_44,
                      votes_45,
                      votes_46,
                      votes_47,
                      votes_48,
                      votes_49,
                      votes_50,
                      votes_51,
                      votes_52,
                      votes_53,
                      votes_54,
                      votes_55,
                      votes_56,
                      votes_57,
                      votes_58,
                      votes_59,
                      votes_60,
                      votes_61,
                      votes_62,
                      votes_63,
                      votes_64,
                      votes_65,
                      votes_66,
                      votes_67,
                      votes_68,
                      votes_69,
                      votes_70,
                      votes_71,
                      votes_72,
                      votes_73,
                      votes_74,
                      votes_75,
                      votes_76,
                      votes_77,
                      votes_78,
                      votes_79,
                      votes_80,
                      votes_81,
                      votes_82,
                      votes_83,
                      votes_84,
                      votes_85,
                      votes_86,
                      votes_87,
                      votes_88,
                      votes_89,
                      votes_90,
                      votes_91,
                      votes_92,
                      votes_93,
                      votes_94,
                      votes_95,
                      votes_96,
                      votes_97,
                      votes_98,
                      votes_99,
                      votes_100,
                      votes_101,
                      votes_102,
                      votes_103,
                      votes_104,
                      votes_105,
                      votes_106,
                      votes_107,
                      votes_108,
                      votes_109,
                      votes_110,
                      votes_111,
                      votes_112,
                      votes_113,
                      votes_114,
                      votes_115,
                      votes_116,
                      votes_117,
                      votes_118,
                      votes_119,
                      votes_120,
                      votes_121,
                      votes_122,
                      votes_123,
                      votes_124,
                      votes_125,
                      votes_126,
                      votes_127,
                      votes_128,
                      votes_129,
                      votes_130,
                      votes_131,
                      votes_132,
                      votes_133,
                      votes_134,
                      votes_135,
                      votes_136,
                      votes_137,
                      votes_138,
                      votes_139,
                      votes_140,
                      votes_141,
                      votes_142,
                      votes_143,
                      votes_144,
                      votes_145,
                      votes_146,
                      votes_147,
                      votes_148,
                      votes_149,
                      votes_150,
                      votes_151,
                      votes_152,
                      votes_153,
                      votes_154,
                      votes_155,
                      votes_156,
                      votes_157,
                      votes_158,
                      votes_159,
                      votes_160,
                      votes_161,
                      votes_162,
                      votes_163,
                      votes_164,
                      votes_165,
                      votes_166,
                      votes_167,
                      votes_168,
                      votes_169,
                      votes_170,
                      votes_171,
                      votes_172,
                      votes_173,
                      votes_174,
                      votes_175,
                      votes_176,
                      votes_177,
                      votes_178,
                      votes_179,
                      votes_180,
                      votes_181,
                      votes_182,
                      votes_183,
                      votes_184,
                      votes_185,
                      votes_186,
                      votes_187,
                      votes_188,
                      votes_189,
                      votes_190,
                      votes_191,
                      votes_192,
                      votes_193,
                      votes_194,
                      votes_195,
                      votes_196,
                      votes_197,
                      votes_198,
                      votes_199,
                      votes_200,
                      votes_201,
                      votes_202,
                      votes_203,
                      votes_204,
                      votes_205,
                      votes_206,
                      votes_207,
                      votes_208,
                      votes_209,
                      votes_210,
                      votes_211,
                      votes_212,
                      votes_213,
                      votes_214,
                      votes_215,
                      votes_216,
                      votes_217,
                      votes_218,
                      votes_219,
                      votes_220,
                      votes_221,
                      votes_222,
                      votes_223,
                      votes_224,
                      votes_225,
                      votes_226,
                      votes_227,
                      votes_228,
                      votes_229,
                      votes_230,
                      votes_231,
                      votes_232,
                      votes_233,
                      votes_234,
                      votes_235,
                      votes_236,
                      votes_237,
                      votes_238,
                      votes_239,
                      votes_240,
                      votes_241,
                      votes_242,
                      votes_243,
                      votes_244,
                      votes_245,
                      votes_246,
                      votes_247,
                      votes_248,
                      votes_249,
                      votes_250,
                      votes_251,
                      votes_252,
                      votes_253,
                      votes_254,
                      votes_255,
                      votes_256,
                      votes_257,
                      votes_258,
                      votes_259,
                      votes_260,
                      votes_261,
                      votes_262,
                      votes_263,
                      votes_264,
                      votes_265,
                      votes_266,
                      votes_267,
                      votes_268,
                      votes_269,
                      votes_270,
                      votes_271,
                      votes_272,
                      votes_273,
                      votes_274,
                      votes_275,
                      votes_276,
                      votes_277,
                      votes_278,
                      votes_279,
                      votes_280,
                      votes_281,
                      votes_282,
                      votes_283,
                      votes_284,
                      votes_285,
                      votes_286,
                      votes_287,
                      votes_288,
                      votes_289,
                      votes_290,
                      votes_291,
                      votes_292,
                      votes_293,
                      votes_294,
                      votes_295,
                      votes_296,
                      votes_297,
                      votes_298,
                      votes_299,
                      votes_300,
                      votes_301,
                      votes_302,
                      votes_303,
                      votes_304,
                      votes_305,
                      votes_306,
                      votes_307,
                      votes_308,
                      votes_309,
                      votes_310,
                      votes_311,
                      votes_312,
                      votes_313,
                      votes_314,
                      votes_315,
                      votes_316,
                      votes_317,
                      votes_318,
                      votes_319,
                      votes_320,
                      votes_321,
                      votes_322,
                      votes_323,
                      votes_324,
                      votes_325,
                      votes_326,
                      votes_327,
                      votes_328,
                      votes_329,
                      votes_330,
                      votes_331,
                      votes_332,
                      votes_333,
                      votes_334,
                      votes_335,
                      votes_336,
                      votes_337,
                      votes_338,
                      votes_339,
                      votes_340,
                      votes_341,
                      votes_342,
                      votes_343,
                      votes_344,
                      votes_345,
                      votes_346,
                      votes_347,
                      votes_348,
                      votes_349,
                      votes_350,
                      votes_351,
                      votes_352,
                      votes_353,
                      votes_354,
                      votes_355,
                      votes_356,
                      votes_357,
                      votes_358,
                      votes_359,
                      votes_360,
                      votes_361,
                      votes_362,
                      votes_363,
                      votes_364,
                      votes_365,
                      votes_366,
                      votes_367,
                      votes_368,
                      votes_369,
                      votes_370,
                      votes_371,
                      votes_372,
                      votes_373,
                      votes_374,
                      votes_375,
                      votes_376,
                      votes_377,
                      votes_378,
                      votes_379,
                      votes_380,
                      votes_381,
                      votes_382,
                      votes_383,
                      votes_384,
                      votes_385,
                      votes_386,
                      votes_387,
                      votes_388,
                      votes_389,
                      votes_390,
                      votes_391,
                      votes_392,
                      votes_393,
                      votes_394,
                      votes_395,
                      votes_396,
                      votes_397,
                      votes_398,
                      votes_399,
                      votes_400,
                      votes_401,
                      votes_402,
                      votes_403,
                      votes_404,
                      votes_405,
                      votes_406,
                      votes_407,
                      votes_408,
                      votes_409,
                      votes_410,
                      votes_411,
                      votes_412,
                      votes_413,
                      votes_414,
                      votes_415,
                      votes_416,
                      votes_417,
                      votes_418,
                      votes_419,
                      votes_420,
                      votes_421,
                      votes_422,
                      votes_423,
                      votes_424,
                      votes_425,
                      votes_426,
                      votes_427,
                      votes_428,
                      votes_429,
                      votes_430,
                      votes_431,
                      votes_432,
                      votes_433,
                      votes_434,
                      votes_435,
                      votes_436,
                      votes_437,
                      votes_438,
                      votes_439,
                      votes_440,
                      votes_441,
                      votes_442,
                      votes_443,
                      votes_444,
                      votes_445,
                      votes_446,
                      votes_447,
                      votes_448,
                      votes_449,
                      votes_450,
                      votes_451,
                      votes_452,
                      votes_453,
                      votes_454,
                      votes_455,
                      votes_456,
                      votes_457,
                      votes_458,
                      votes_459,
                      votes_460,
                      votes_461,
                      votes_462,
                      votes_463,
                      votes_464,
                      votes_465,
                      votes_466,
                      votes_467,
                      votes_468,
                      votes_469,
                      votes_470,
                      votes_471,
                      votes_472,
                      votes_473,
                      votes_474,
                      votes_475,
                      votes_476,
                      votes_477,
                      votes_478,
                      votes_479,
                      votes_480,
                      votes_481,
                      votes_482,
                      votes_483,
                      votes_484,
                      votes_485,
                      votes_486,
                      votes_487,
                      votes_488,
                      votes_489,
                      votes_490,
                      votes_491,
                      votes_492,
                      votes_493,
                      votes_494,
                      votes_495,
                      votes_496,
                      votes_497,
                      votes_498,
                      votes_499,
                      votes_500,
                      votes_501,
                      votes_502,
                      votes_503,
                      votes_504,
                      votes_505,
                      votes_506,
                      votes_507,
                      votes_508,
                      votes_509,
                      votes_510,
                      votes_511,
                      votes_512,
                      votes_513,
                      votes_514,
                      votes_515,
                      votes_516,
                      votes_517,
                      votes_518,
                      votes_519)

## Social Network Edge Set

merged_votes <- merge(total_votes, total_votes, by.x = "vote",by.y = "vote") %>% filter(names.x != names.y) %>% group_by(names.x, names.y) %>% summarise(shared_votes = n()) %>% ungroup() %>% filter(names.x > names.y) 

head(merged_votes)

