/*****************************************************/                                                */
/*  Kun, Yang   301178299                            */
/*  Stat 340, Spring 2013                            */
/*  Assignment 1, Part 3  - Atus                     */
/*                                                   */
/*  Read in the cereal dataset, do some simple plots,*/
/*  and then do a regression of number of calores    */
/*  vs. grams of fat.                                */
/*                                                   */
/*  Change log:                                      */
/*    2015-01-09 KY First Edition                    */
/*****************************************************/

/* Assignment 1, Part 3 - Analysis of Atus */

dm 'output' clear;
dm 'log'    clear;
proc datasets kill;
footnote;

/* Label each page and put explanatory notes*/
title justify=left 'Yang, Kun 301178299'
      justify=center 'Assignment 1 - Part 3';
title2 'Atus Dataset';

/* Convert the output to a PDF file*/
ods pdf file='a01p01-atus.pdf' style=styles.printer;



/* Read the data into SAS*/
* NOTE: format names are the same as variable names, except for
        variable names that end in a number.  For these a 1 is replaced
        by an A, a 2 is replaced by a B, and so on.
  Edit the infile statement to reference the data file on your computer.
*;
data atussum_0313;
infile 'atussum_0313.csv' firstobs=2 dsd missover lrecl=16384 dlm=",";
length
TUCASEID $14
GEMETSTA 8
GTMETSTA 8
PEEDUCA 8
PEHSPNON 8
PTDTRACE 8
TEAGE 8
TELFS 8
TEMJOT 8
TESCHENR 8
TESCHLVL 8
TESEX 8
TESPEMPNOT 8
TRCHILDNUM 8
TRDPFTPT 8
TRERNWA 8
TRHOLIDAY 8
TRSPFTPT 8
TRSPPRES 8
TRYHHCHILD 8
TUDIARYDAY 8
TUFNWGTP 8
TEHRUSLT 8
TUYEAR 8
t010101 8
t010102 8
t010199 8
t010201 8
t010299 8
t010301 8
t010399 8
t010401 8
t010499 8
t010501 8
t010599 8
t019999 8
t020101 8
t020102 8
t020103 8
t020104 8
t020199 8
t020201 8
t020202 8
t020203 8
t020299 8
t020301 8
t020302 8
t020303 8
t020399 8
t020401 8
t020402 8
t020499 8
t020501 8
t020502 8
t020599 8
t020681 8
t020699 8
t020701 8
t020799 8
t020801 8
t020899 8
t020901 8
t020902 8
t020903 8
t020904 8
t020905 8
t020999 8
t029999 8
t030101 8
t030102 8
t030103 8
t030104 8
t030105 8
t030108 8
t030109 8
t030110 8
t030111 8
t030112 8
t030186 8
t030199 8
t030201 8
t030202 8
t030203 8
t030204 8
t030299 8
t030301 8
t030302 8
t030303 8
t030399 8
t030401 8
t030402 8
t030403 8
t030404 8
t030405 8
t030499 8
t030501 8
t030502 8
t030503 8
t030504 8
t030599 8
t039999 8
t040101 8
t040102 8
t040103 8
t040104 8
t040105 8
t040108 8
t040109 8
t040110 8
t040111 8
t040112 8
t040186 8
t040199 8
t040201 8
t040202 8
t040203 8
t040204 8
t040299 8
t040301 8
t040302 8
t040303 8
t040399 8
t040401 8
t040402 8
t040403 8
t040404 8
t040405 8
t040499 8
t040501 8
t040502 8
t040503 8
t040504 8
t040505 8
t040506 8
t040507 8
t040508 8
t040599 8
t049999 8
t050101 8
t050102 8
t050103 8
t050189 8
t050201 8
t050202 8
t050203 8
t050204 8
t050289 8
t050301 8
t050302 8
t050303 8
t050304 8
t050389 8
t050403 8
t050404 8
t050405 8
t050481 8
t050499 8
t059999 8
t060101 8
t060102 8
t060103 8
t060104 8
t060199 8
t060201 8
t060202 8
t060203 8
t060289 8
t060301 8
t060302 8
t060303 8
t060399 8
t060401 8
t060402 8
t060403 8
t060499 8
t069999 8
t070101 8
t070102 8
t070103 8
t070104 8
t070105 8
t070199 8
t070201 8
t070299 8
t070301 8
t070399 8
t079999 8
t080101 8
t080102 8
t080199 8
t080201 8
t080202 8
t080203 8
t080299 8
t080301 8
t080302 8
t080399 8
t080401 8
t080402 8
t080403 8
t080499 8
t080501 8
t080502 8
t080599 8
t080601 8
t080602 8
t080699 8
t080701 8
t080702 8
t080799 8
t080801 8
t080899 8
t089999 8
t090101 8
t090102 8
t090103 8
t090104 8
t090199 8
t090201 8
t090202 8
t090299 8
t090301 8
t090302 8
t090399 8
t090401 8
t090402 8
t090499 8
t090501 8
t090502 8
t090599 8
t099999 8
t100101 8
t100102 8
t100103 8
t100199 8
t100201 8
t100299 8
t100381 8
t100383 8
t100399 8
t100401 8
t100499 8
t109999 8
t110101 8
t110199 8
t110281 8
t110289 8
t119999 8
t120101 8
t120199 8
t120201 8
t120202 8
t120299 8
t120301 8
t120302 8
t120303 8
t120304 8
t120305 8
t120306 8
t120307 8
t120308 8
t120309 8
t120310 8
t120311 8
t120312 8
t120313 8
t120399 8
t120401 8
t120402 8
t120403 8
t120404 8
t120405 8
t120499 8
t120501 8
t120502 8
t120503 8
t120504 8
t120599 8
t129999 8
t130101 8
t130102 8
t130103 8
t130104 8
t130105 8
t130106 8
t130107 8
t130108 8
t130109 8
t130110 8
t130111 8
t130112 8
t130113 8
t130114 8
t130115 8
t130116 8
t130117 8
t130118 8
t130119 8
t130120 8
t130121 8
t130122 8
t130123 8
t130124 8
t130125 8
t130126 8
t130127 8
t130128 8
t130129 8
t130130 8
t130131 8
t130132 8
t130133 8
t130134 8
t130135 8
t130136 8
t130199 8
t130201 8
t130202 8
t130203 8
t130204 8
t130205 8
t130206 8
t130207 8
t130208 8
t130209 8
t130210 8
t130211 8
t130212 8
t130213 8
t130214 8
t130215 8
t130216 8
t130217 8
t130218 8
t130219 8
t130220 8
t130221 8
t130222 8
t130223 8
t130224 8
t130225 8
t130226 8
t130227 8
t130228 8
t130229 8
t130230 8
t130231 8
t130232 8
t130299 8
t130301 8
t130302 8
t130399 8
t130401 8
t130402 8
t130499 8
t139999 8
t140101 8
t140102 8
t140103 8
t140104 8
t140105 8
t149999 8
t150101 8
t150102 8
t150103 8
t150104 8
t150105 8
t150106 8
t150199 8
t150201 8
t150202 8
t150203 8
t150204 8
t150299 8
t150301 8
t150302 8
t150399 8
t150401 8
t150402 8
t150499 8
t150501 8
t150599 8
t150601 8
t150602 8
t150699 8
t159989 8
t160101 8
t160102 8
t160103 8
t160104 8
t160105 8
t160106 8
t160107 8
t160108 8
t169989 8
t180101 8
t180199 8
t180280 8
t180381 8
t180382 8
t180399 8
t180481 8
t180482 8
t180499 8
t180501 8
t180502 8
t180589 8
t180601 8
t180682 8
t180699 8
t180701 8
t180782 8
t180801 8
t180802 8
t180803 8
t180804 8
t180805 8
t180806 8
t180807 8
t180899 8
t180901 8
t180902 8
t180903 8
t180904 8
t180905 8
t180999 8
t181002 8
t181081 8
t181099 8
t181101 8
t181199 8
t181201 8
t181202 8
t181204 8
t181283 8
t181299 8
t181301 8
t181302 8
t181399 8
t181401 8
t181499 8
t181501 8
t181599 8
t181601 8
t181699 8
t181801 8
t181899 8
t189999 8
t500101 8
t500103 8
t500104 8
t500105 8
t500106 8
t500107 8
t509989 8
;

input
TUCASEID
GEMETSTA
GTMETSTA
PEEDUCA
PEHSPNON
PTDTRACE
TEAGE
TELFS
TEMJOT
TESCHENR
TESCHLVL
TESEX
TESPEMPNOT
TRCHILDNUM
TRDPFTPT
TRERNWA
TRHOLIDAY
TRSPFTPT
TRSPPRES
TRYHHCHILD
TUDIARYDAY
TUFNWGTP
TEHRUSLT
TUYEAR
t010101
t010102
t010199
t010201
t010299
t010301
t010399
t010401
t010499
t010501
t010599
t019999
t020101
t020102
t020103
t020104
t020199
t020201
t020202
t020203
t020299
t020301
t020302
t020303
t020399
t020401
t020402
t020499
t020501
t020502
t020599
t020681
t020699
t020701
t020799
t020801
t020899
t020901
t020902
t020903
t020904
t020905
t020999
t029999
t030101
t030102
t030103
t030104
t030105
t030108
t030109
t030110
t030111
t030112
t030186
t030199
t030201
t030202
t030203
t030204
t030299
t030301
t030302
t030303
t030399
t030401
t030402
t030403
t030404
t030405
t030499
t030501
t030502
t030503
t030504
t030599
t039999
t040101
t040102
t040103
t040104
t040105
t040108
t040109
t040110
t040111
t040112
t040186
t040199
t040201
t040202
t040203
t040204
t040299
t040301
t040302
t040303
t040399
t040401
t040402
t040403
t040404
t040405
t040499
t040501
t040502
t040503
t040504
t040505
t040506
t040507
t040508
t040599
t049999
t050101
t050102
t050103
t050189
t050201
t050202
t050203
t050204
t050289
t050301
t050302
t050303
t050304
t050389
t050403
t050404
t050405
t050481
t050499
t059999
t060101
t060102
t060103
t060104
t060199
t060201
t060202
t060203
t060289
t060301
t060302
t060303
t060399
t060401
t060402
t060403
t060499
t069999
t070101
t070102
t070103
t070104
t070105
t070199
t070201
t070299
t070301
t070399
t079999
t080101
t080102
t080199
t080201
t080202
t080203
t080299
t080301
t080302
t080399
t080401
t080402
t080403
t080499
t080501
t080502
t080599
t080601
t080602
t080699
t080701
t080702
t080799
t080801
t080899
t089999
t090101
t090102
t090103
t090104
t090199
t090201
t090202
t090299
t090301
t090302
t090399
t090401
t090402
t090499
t090501
t090502
t090599
t099999
t100101
t100102
t100103
t100199
t100201
t100299
t100381
t100383
t100399
t100401
t100499
t109999
t110101
t110199
t110281
t110289
t119999
t120101
t120199
t120201
t120202
t120299
t120301
t120302
t120303
t120304
t120305
t120306
t120307
t120308
t120309
t120310
t120311
t120312
t120313
t120399
t120401
t120402
t120403
t120404
t120405
t120499
t120501
t120502
t120503
t120504
t120599
t129999
t130101
t130102
t130103
t130104
t130105
t130106
t130107
t130108
t130109
t130110
t130111
t130112
t130113
t130114
t130115
t130116
t130117
t130118
t130119
t130120
t130121
t130122
t130123
t130124
t130125
t130126
t130127
t130128
t130129
t130130
t130131
t130132
t130133
t130134
t130135
t130136
t130199
t130201
t130202
t130203
t130204
t130205
t130206
t130207
t130208
t130209
t130210
t130211
t130212
t130213
t130214
t130215
t130216
t130217
t130218
t130219
t130220
t130221
t130222
t130223
t130224
t130225
t130226
t130227
t130228
t130229
t130230
t130231
t130232
t130299
t130301
t130302
t130399
t130401
t130402
t130499
t139999
t140101
t140102
t140103
t140104
t140105
t149999
t150101
t150102
t150103
t150104
t150105
t150106
t150199
t150201
t150202
t150203
t150204
t150299
t150301
t150302
t150399
t150401
t150402
t150499
t150501
t150599
t150601
t150602
t150699
t159989
t160101
t160102
t160103
t160104
t160105
t160106
t160107
t160108
t169989
t180101
t180199
t180280
t180381
t180382
t180399
t180481
t180482
t180499
t180501
t180502
t180589
t180601
t180682
t180699
t180701
t180782
t180801
t180802
t180803
t180804
t180805
t180806
t180807
t180899
t180901
t180902
t180903
t180904
t180905
t180999
t181002
t181081
t181099
t181101
t181199
t181201
t181202
t181204
t181283
t181299
t181301
t181302
t181399
t181401
t181499
t181501
t181599
t181601
t181699
t181801
t181899
t189999
t500101
t500103
t500104
t500105
t500106
t500107
t509989
;

label TUCASEID = "ATUS Case ID (14-digit identifier)";
label PEEDUCA = "Edited: what is the highest level of school you have completed or the highest degree you have received?";
label PTDTRACE = "Race (topcoded)";
label GTMETSTA = "Metropolitan status (2000 definitions)";
label PEHSPNON = "Edited: are you Spanish, Hispanic, or Latino?";
label GEMETSTA = "Metropolitan status (1990 definitions)";
label TRCHILDNUM = "Number of household children < 18";
label TUDIARYDAY = "Day of the week of diary day (day of the week about which the respondent was interviewed)";
label TRERNWA = "Weekly earnings (2 implied decimals)";
label TRHOLIDAY = "Flag to indicate if diary day was a holiday";
label TRSPFTPT = "Full time or part time employment status of spouse or unmarried partner";
label TRSPPRES = "Presence of the respondent's spouse or unmarried partner in the household";
label TRDPFTPT = "Full time or part time employment status of respondent";
label TUFNWGTP = "ATUS final weight";
label TESPEMPNOT = "Edited: employment status of spouse or unmarried partner";
label TESCHLVL = "Edited: would that be high school, college, or university?";
label TESCHENR = "Edited: are you enrolled in high school, college, or university?";
label TEMJOT = "Edited: in the last seven days did you have more than one job?";
label TELFS = "Edited: labor force status";
label TEHRUSLT = "Edited: total hours usually worked per week (sum of TEHRUSL1 and TEHRUSL2)";
label TRYHHCHILD = "Age of youngest household child < 18";
label TEAGE = "Edited: age";
label TESEX = "Edited: sex";
label TUYEAR = "Year of diary day (year of day about which respondent was interviewed)";
label t010101 = "Sleeping";
label t010102 = "Sleeplessness";
label t010199 = "Sleeping, n.e.c.*";
label t010201 = "Washing, dressing and grooming oneself";
label t010299 = "Grooming, n.e.c.*";
label t010301 = "Health-related self care";
label t010399 = "Self care, n.e.c.*";
label t010401 = "Personal/Private activities";
label t010499 = "Personal activities, n.e.c.*";
label t010501 = "Personal emergencies";
label t010599 = "Personal care emergencies, n.e.c.*";
label t019999 = "Personal care, n.e.c.*";
label t020101 = "Interior cleaning";
label t020102 = "Laundry";
label t020103 = "Sewing, repairing, & maintaining textiles";
label t020104 = "Storing interior hh items, inc. food";
label t020199 = "Housework, n.e.c.* ";
label t020201 = "Food and drink preparation ";
label t020202 = "Food presentation";
label t020203 = "Kitchen and food clean-up";
label t020299 = "Food & drink prep, presentation, & clean-up, n.e.c.* ";
label t020301 = "Interior arrangement, decoration, & repairs";
label t020302 = "Building and repairing furniture";
label t020303 = "Heating and cooling";
label t020399 = "Interior maintenance, repair, & decoration, n.e.c.* ";
label t020401 = "Exterior cleaning";
label t020402 = "Exterior repair, improvements, & decoration";
label t020499 = "Exterior maintenance, repair & decoration, n.e.c.*";
label t020501 = "Lawn, garden, and houseplant care";
label t020502 = "Ponds, pools, and hot tubs";
label t020599 = "Lawn and garden, n.e.c.* ";
label t020681 = "Care for animals and pets (not veterinary care)";
label t020699 = "Pet and animal care, n.e.c.*";
label t020701 = "Vehicle repair and maintenance (by self)";
label t020799 = "Vehicles, n.e.c.*";
label t020801 = "Appliance, tool, and toy set-up, repair, & maintenance (by self)";
label t020899 = "Appliances and tools, n.e.c.*";
label t020901 = "Financial management";
label t020902 = "Household & personal organization and planning";
label t020903 = "HH & personal mail & messages (except e-mail)";
label t020904 = "HH & personal e-mail and messages";
label t020905 = "Home security";
label t020999 = "Household management, n.e.c.*";
label t029999 = "Household activities, n.e.c.*";
label t030101 = "Physical care for hh children";
label t030102 = "Reading to/with hh children";
label t030103 = "Playing with hh children, not sports";
label t030104 = "Arts and crafts with hh children";
label t030105 = "Playing sports with hh children";
label t030186 = "Talking with/listening to hh children";
label t030108 = "Organization & planning for hh children";
label t030109 = "Looking after hh children (as a primary activity)";
label t030110 = "Attending hh children's events";
label t030111 = "Waiting for/with hh children";
label t030112 = "Picking up/dropping off hh children";
label t030199 = "Caring for & helping hh children, n.e.c.*";
label t030201 = "Homework (hh children)";
label t030202 = "Meetings and school conferences (hh children)";
label t030203 = "Home schooling of hh children";
label t030204 = "Waiting associated with hh children's education";
label t030299 = "Activities related to hh child's education, n.e.c.*";
label t030301 = "Providing medical care to hh children";
label t030302 = "Obtaining medical care for hh children";
label t030303 = "Waiting associated with hh children's health";
label t030399 = "Activities related to hh child's health, n.e.c.*";
label t030401 = "Physical care for hh adults";
label t030402 = "Looking after hh adult (as a primary activity)";
label t030403 = "Providing medical care to hh adult";
label t030404 = "Obtaining medical and care services for hh adult";
label t030405 = "Waiting associated with caring for household adults";
label t030499 = "Caring for household adults, n.e.c.* ";
label t030501 = "Helping hh adults";
label t030502 = "Organization & planning for hh adults";
label t030503 = "Picking up/dropping off hh adult";
label t030504 = "Waiting associated with helping hh adults";
label t030599 = "Helping household adults, n.e.c.*";
label t039999 = "Caring for & helping hh members, n.e.c.*";
label t040101 = "Physical care for nonhh children";
label t040102 = "Reading to/with nonhh children";
label t040103 = "Playing with nonhh children, not sports";
label t040104 = "Arts and crafts with nonhh children";
label t040105 = "Playing sports with nonhh children";
label t040186 = "Talking with/listening to nonhh children";
label t040108 = "Organization & planning for nonhh children";
label t040109 = "Looking after nonhh children (as primary activity)";
label t040110 = "Attending nonhh children's events";
label t040111 = "Waiting for/with nonhh children";
label t040112 = "Dropping off/picking up nonhh children";
label t040199 = "Caring for and helping nonhh children, n.e.c.*";
label t040201 = "Homework (nonhh children)";
label t040202 = "Meetings and school conferences (nonhh children)";
label t040203 = "Home schooling of nonhh children";
label t040204 = "Waiting associated with nonhh children's education";
label t040299 = "Activities related to nonhh child's educ., n.e.c.*";
label t040301 = "Providing medical care to nonhh children";
label t040302 = "Obtaining medical care for nonhh children";
label t040303 = "Waiting associated with nonhh children's health";
label t040399 = "Activities related to nonhh child's health, n.e.c.*";
label t040401 = "Physical care for nonhh adults";
label t040402 = "Looking after nonhh adult (as a primary activity)";
label t040403 = "Providing medical care to nonhh adult";
label t040404 = "Obtaining medical and care services for nonhh adult";
label t040405 = "Waiting associated with caring for nonhh adults";
label t040499 = "Caring for nonhh adults, n.e.c.*";
label t040501 = "Housework, cooking, & shopping assistance for nonhh adults";
label t040502 = "House & lawn maintenance & repair assistance for nonhh adults";
label t040503 = "Animal & pet care assistance for nonhh adults";
label t040504 = "Vehicle & appliance maintenance/repair assistance for nonhh adults";
label t040505 = "Financial management assistance for nonhh adults";
label t040506 = "Household management & paperwork assistance for nonhh adults";
label t040507 = "Picking up/dropping off nonhh adult";
label t040508 = "Waiting associated with helping nonhh adults";
label t040599 = "Helping nonhh adults, n.e.c.*";
label t049999 = "Caring for & helping nonhh members, n.e.c.*";
label t050101 = "Work, main job";
label t050102 = "Work, other job(s)";
label t050103 = "Security procedures related to work";
label t050189 = "Working, n.e.c.*";
label t050201 = "Socializing, relaxing, and leisure as part of job";
label t050202 = "Eating and drinking as part of job";
label t050203 = "Sports and exercise as part of job";
label t050204 = "Security procedures as part of job";
label t050289 = "Work-related activities, n.e.c.*";
label t050301 = "Income-generating hobbies, crafts, and food";
label t050302 = "Income-generating performances ";
label t050303 = "Income-generating services ";
label t050304 = "Income-generating rental property activities";
label t050389 = "Other income-generating activities, n.e.c.*";
label t050481 = "Job search activities";
label t050403 = "Job interviewing ";
label t050404 = "Waiting associated with job search or interview";
label t050405 = "Security procedures rel. to job search/interviewing";
label t050499 = "Job search and Interviewing, n.e.c.*";
label t059999 = "Work and work-related activities, n.e.c.*";
label t060101 = "Taking class for degree, certification, or licensure";
label t060102 = "Taking class for personal interest";
label t060103 = "Waiting associated with taking classes";
label t060104 = "Security procedures rel. to taking classes";
label t060199 = "Taking class, n.e.c.* ";
label t060201 = "Extracurricular club activities";
label t060202 = "Extracurricular music & performance activities";
label t060203 = "Extracurricular student government activities";
label t060289 = "Education-related extracurricular activities, n.e.c.*";
label t060301 = "Research/homework for class for degree, certification, or licensure";
label t060302 = "Research/homework for class for pers. interest";
label t060303 = "Waiting associated with research/homework";
label t060399 = "Research/homework n.e.c.*";
label t060401 = "Administrative activities: class for degree, certification, or licensure";
label t060402 = "Administrative activities: class for personal interest";
label t060403 = "Waiting associated w/admin. activities (education)";
label t060499 = "Administrative for education, n.e.c.*";
label t069999 = "Education, n.e.c.*";
label t070101 = "Grocery shopping";
label t070102 = "Purchasing gas";
label t070103 = "Purchasing food (not groceries)";
label t070104 = "Shopping, except groceries, food and gas";
label t070105 = "Waiting associated with shopping";
label t070199 = "Shopping, n.e.c.*";
label t070201 = "Comparison shopping";
label t070299 = "Researching purchases, n.e.c.*";
label t070301 = "Security procedures rel. to consumer purchases";
label t070399 = "Security procedures rel. to consumer purchases, n.e.c.*";
label t079999 = "Consumer purchases, n.e.c.*";
label t080101 = "Using paid childcare services";
label t080102 = "Waiting associated w/purchasing childcare svcs";
label t080199 = "Using paid childcare services, n.e.c.*";
label t080201 = "Banking";
label t080202 = "Using other financial services";
label t080203 = "Waiting associated w/banking/financial services";
label t080299 = "Using financial services and banking, n.e.c.*";
label t080301 = "Using legal services";
label t080302 = "Waiting associated with legal services";
label t080399 = "Using legal services, n.e.c.*";
label t080401 = "Using health and care services outside the home";
label t080402 = "Using in-home health and care services";
label t080403 = "Waiting associated with medical services";
label t080499 = "Using medical services, n.e.c.*";
label t080501 = "Using personal care services";
label t080502 = "Waiting associated w/personal care services";
label t080599 = "Using personal care services, n.e.c.*";
label t080601 = "Activities rel. to purchasing/selling real estate";
label t080602 = "Waiting associated w/purchasing/selling real estate";
label t080699 = "Using real estate services, n.e.c.*";
label t080701 = "Using veterinary services";
label t080702 = "Waiting associated with veterinary services";
label t080799 = "Using veterinary services, n.e.c.*";
label t080801 = "Security procedures rel. to professional/personal svcs.";
label t080899 = "Security procedures rel. to professional/personal svcs n.e.c.*";
label t089999 = "Professional and personal services, n.e.c.*";
label t090101 = "Using interior cleaning services ";
label t090102 = "Using meal preparation services";
label t090103 = "Using clothing repair and cleaning services";
label t090104 = "Waiting associated with using household services";
label t090199 = "Using household services, n.e.c.*";
label t090201 = "Using home maint/repair/d?cor/construction svcs";
label t090202 = "Waiting associated w/ home main/repair/d?cor/constr";
label t090299 = "Using home maint/repair/d?cor/constr services, n.e.c.*";
label t090301 = "Using pet services";
label t090302 = "Waiting associated with pet services";
label t090399 = "Using pet services, n.e.c.*";
label t090401 = "Using lawn and garden services";
label t090402 = "Waiting associated with using lawn & garden services";
label t090499 = "Using lawn and garden services, n.e.c.*";
label t090501 = "Using vehicle maintenance or repair services";
label t090502 = "Waiting associated with vehicle main. or repair svcs";
label t090599 = "Using vehicle maint. & repair svcs, n.e.c.*";
label t099999 = "Using household services, n.e.c.*";
label t100101 = "Using police and fire services";
label t100102 = "Using social services";
label t100103 = "Obtaining licenses & paying fines, fees, taxes";
label t100199 = "Using government services, n.e.c.*";
label t100201 = "Civic obligations & participation";
label t100299 = "Civic obligations & participation, n.e.c.*";
label t100381 = "Waiting associated w/ using government services";
label t100383 = "Waiting associated w/civic obligations & participation";
label t100399 = "Waiting assoc. w/govt svcs or civic obligations, n.e.c.*";
label t100401 = "Security procedures rel. to govt svcs/civic obligations";
label t100499 = "Security procedures rel. to govt svcs/civic obligations, n.e.c.*";
label t109999 = "Government services, n.e.c.*";
label t110101 = "Eating and drinking";
label t110199 = "Eating and drinking, n.e.c.*";
label t110281 = "Waiting associated w/eating & drinking";
label t110289 = "Waiting associated with eating & drinking, n.e.c.*";
label t119999 = "Eating and drinking, n.e.c.*";
label t120101 = "Socializing and communicating with others";
label t120199 = "Socializing and communicating, n.e.c.*";
label t120201 = "Attending or hosting parties/receptions/ceremonies";
label t120202 = "Attending meetings for personal interest (not volunteering)";
label t120299 = "Attending/hosting social events, n.e.c.*";
label t120301 = "Relaxing, thinking ";
label t120302 = "Tobacco and drug use";
label t120303 = "Television and movies (not religious)";
label t120304 = "Television (religious)";
label t120305 = "Listening to the radio";
label t120306 = "Listening to/playing music (not radio)";
label t120307 = "Playing games";
label t120308 = "Computer use for leisure (exc. Games)";
label t120309 = "Arts and crafts as a hobby";
label t120310 = "Collecting as a hobby";
label t120311 = "Hobbies, except arts & crafts and collecting";
label t120312 = "Reading for personal interest";
label t120313 = "Writing for personal interest ";
label t120399 = "Relaxing and leisure, n.e.c.*";
label t120401 = "Attending performing arts";
label t120402 = "Attending museums";
label t120403 = "Attending movies/film";
label t120404 = "Attending gambling establishments";
label t120405 = "Security procedures rel. to arts & entertainment";
label t120499 = "Arts and entertainment, n.e.c.*";
label t120501 = "Waiting assoc. w/socializing & communicating";
label t120502 = "Waiting assoc. w/attending/hosting social events";
label t120503 = "Waiting associated with relaxing/leisure";
label t120504 = "Waiting associated with arts & entertainment";
label t120599 = "Waiting associated with socializing, n.e.c.*";
label t129999 = "Socializing, relaxing, and leisure, n.e.c.*";
label t130101 = "Doing aerobics";
label t130102 = "Playing baseball";
label t130103 = "Playing basketball";
label t130104 = "Biking";
label t130105 = "Playing billiards";
label t130106 = "Boating";
label t130107 = "Bowling ";
label t130108 = "Climbing, spelunking, caving";
label t130109 = "Dancing";
label t130110 = "Participating in equestrian sports";
label t130111 = "Fencing";
label t130112 = "Fishing";
label t130113 = "Playing football";
label t130114 = "Golfing";
label t130115 = "Doing gymnastics";
label t130116 = "Hiking";
label t130117 = "Playing hockey";
label t130118 = "Hunting";
label t130119 = "Participating in martial arts";
label t130120 = "Playing racquet sports ";
label t130121 = "Participating in rodeo competitions";
label t130122 = "Rollerblading";
label t130123 = "Playing rugby";
label t130124 = "Running";
label t130125 = "Skiing, ice skating, snowboarding";
label t130126 = "Playing soccer";
label t130127 = "Softball";
label t130128 = "Using cardiovascular equipment";
label t130129 = "Vehicle touring/racing";
label t130130 = "Playing volleyball";
label t130131 = "Walking";
label t130132 = "Participating in water sports";
label t130133 = "Weightlifting/strength training";
label t130134 = "Working out, unspecified";
label t130135 = "Wrestling";
label t130136 = "Doing yoga";
label t130199 = "Playing sports n.e.c.*";
label t130201 = "Watching aerobics";
label t130202 = "Watching baseball";
label t130203 = "Watching basketball";
label t130204 = "Watching biking";
label t130205 = "Watching billiards";
label t130206 = "Watching boating";
label t130207 = "Watching bowling";
label t130208 = "Watching climbing, spelunking, caving";
label t130209 = "Watching dancing";
label t130210 = "Watching equestrian sports";
label t130211 = "Watching fencing";
label t130212 = "Watching fishing";
label t130213 = "Watching football";
label t130214 = "Watching golfing";
label t130215 = "Watching gymnastics";
label t130216 = "Watching hockey";
label t130217 = "Watching martial arts ";
label t130218 = "Watching racquet sports";
label t130219 = "Watching rodeo competitions";
label t130220 = "Watching rollerblading";
label t130221 = "Watching rugby";
label t130222 = "Watching running";
label t130223 = "Watching skiing, ice skating, snowboarding";
label t130224 = "Watching soccer";
label t130225 = "Watching softball";
label t130226 = "Watching vehicle touring/racing";
label t130227 = "Watching volleyball";
label t130228 = "Watching walking";
label t130229 = "Watching water sports";
label t130230 = "Watching weightlifting/strength training";
label t130231 = "Watching people working out, unspecified";
label t130232 = "Watching wrestling";
label t130299 = "Attending sporting events, n.e.c.*";
label t130301 = "Waiting related to playing sports or exercising";
label t130302 = "Waiting related to attending sporting events";
label t130399 = "Waiting associated with sports, exercise, & recreation, n.e.c.*";
label t130401 = "Security related to playing sports or exercising";
label t130402 = "Security related to attending sporting events";
label t130499 = "Security related to sports, exercise, & recreation, n.e.c.*";
label t139999 = "Sports, exercise, & recreation, n.e.c.*";
label t140101 = "Attending religious services";
label t140102 = "Participation in religious practices";
label t140103 = "Waiting associated w/religious & spiritual activities";
label t140104 = "Security procedures rel. to religious & spiritual activities";
label t140105 = "Religious education activities";
label t149999 = "Religious and spiritual activities, n.e.c.*";
label t150101 = "Computer use";
label t150102 = "Organizing and preparing";
label t150103 = "Reading";
label t150104 = "Telephone calls (except hotline counseling)";
label t150105 = "Writing";
label t150106 = "Fundraising";
label t150199 = "Administrative & support activities, n.e.c.*";
label t150201 = "Food preparation, presentation, clean-up";
label t150202 = "Collecting & delivering clothing & other goods";
label t150203 = "Providing care";
label t150204 = "Teaching, leading, counseling, mentoring";
label t150299 = "Social service & care activities, n.e.c.*";
label t150301 = "Building houses, wildlife sites, & other structures";
label t150302 = "Indoor & outdoor maintenance, repair, & clean-up";
label t150399 = "Indoor & outdoor maintenance, building & clean-up activities, n.e.c.*";
label t150401 = "Performing";
label t150402 = "Serving at volunteer events & cultural activities";
label t150499 = "Participating in performance & cultural activities, n.e.c.*";
label t150501 = "Attending meetings, conferences, & training";
label t150599 = "Attending meetings, conferences, & training, n.e.c.*";
label t150601 = "Public health activities";
label t150602 = "Public safety activities";
label t150699 = "Public health & safety activities, n.e.c.*";
label t159989 = "Volunteer activities, n.e.c.*";
label t160101 = "Telephone calls to/from family members";
label t160102 = "Telephone calls to/from friends, neighbors, or acquaintances";
label t160103 = "Telephone calls to/from education services providers";
label t160104 = "Telephone calls to/from salespeople";
label t160105 = "Telephone calls to/from professional or personal care svcs providers";
label t160106 = "Telephone calls to/from household services providers";
label t160107 = "Telephone calls to/from paid child or adult care providers";
label t160108 = "Telephone calls to/from government officials";
label t169989 = "Telephone calls, n.e.c.*";
label t180101 = "Travel related to personal care";
label t180199 = "Travel related to personal care, n.e.c.*";
label t180280 = "Travel related to household activities   ";
label t180381 = "Travel related to caring for and helping hh children";
label t180382 = "Travel related to caring for and helping hh adults";
label t180399 = "Travel rel. to caring for & helping hh members, n.e.c.*";
label t180481 = "Travel related to caring for and helping nonhh children";
label t180482 = "Travel related to caring for and helping nonhh adults";
label t180499 = "Travel rel. to caring for & helping nonhh members, n.e.c.*";
label t180501 = "Travel related to working";
label t180502 = "Travel related to work-related activities";
label t180589 = "Travel related to work, n.e.c.*";
label t180601 = "Travel related to taking class";
label t180682 = "Travel related to education (except taking class)";
label t180699 = "Travel related to education, n.e.c.*";
label t180701 = "Travel related to grocery shopping";
label t180782 = "Travel related to shopping (except grocery shopping)";
label t180801 = "Travel related to using childcare services";
label t180802 = "Travel related to using financial services and banking";
label t180803 = "Travel related to using legal services";
label t180804 = "Travel related to using medical services";
label t180805 = "Travel related to using personal care services";
label t180806 = "Travel related to using real estate services";
label t180807 = "Travel related to using veterinary services";
label t180899 = "Travel rel. to using prof. & personal care services, n.e.c.*";
label t180901 = "Travel related to using household services";
label t180902 = "Travel related to using home main./repair/d?cor./construction svcs";
label t180903 = "Travel related to using pet services (not vet)";
label t180904 = "Travel related to using lawn and garden services";
label t180905 = "Travel related to using vehicle maintenance & repair services";
label t180999 = "Travel related to using household services, n.e.c.*";
label t181081 = "Travel related to using government services";
label t181002 = "Travel related to civic obligations & participation";
label t181099 = "Travel rel. to govt svcs & civic obligations, n.e.c.*";
label t181101 = "Travel related to eating and drinking";
label t181199 = "Travel related to eating and drinking, n.e.c.*";
label t181201 = "Travel related to socializing and communicating";
label t181202 = "Travel related to attending or hosting social events";
label t181283 = "Travel related to relaxing and leisure";
label t181204 = "Travel related to arts and entertainment";
label t181299 = "Travel rel. to socializing, relaxing, & leisure, n.e.c.*";
label t181301 = "Travel related to participating in sports/exercise/recreation";
label t181302 = "Travel related to attending sporting/recreational events";
label t181399 = "Travel related to sports, exercise, & recreation, n.e.c.*";
label t181401 = "Travel related to religious/spiritual practices";
label t181499 = "Travel rel. to religious/spiritual activities, n.e.c.*";
label t181501 = "Travel related to volunteering";
label t181599 = "Travel related to volunteer activities, n.e.c.*";
label t181601 = "Travel related to phone calls";
label t181699 = "Travel rel. to phone calls, n.e.c.*";
label t181801 = "Security procedures related to traveling";
label t181899 = "Security procedures related to traveling, n.e.c.*";
label t189999 = "Traveling, n.e.c.*";
label t500101 = "Insufficient detail in verbatim";
label t500103 = "Missing travel or destination";
label t500104 = "Recorded simultaneous activities incorrectly";
label t500105 = "Respondent refused to provide information/none of your business";
label t500106 = "Gap/can't remember";
label t500107 = "Unable to code activity at 1st tier";
label t509989 = "Data codes, n.e.c.*";
run;

proc format;
value GEMETSTA
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 =     "Metropolitan"
2 = "Non-metropolitan"
3 =     "Not identified"
;
value GTMETSTA
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 =     "Metropolitan"
2 = "Non-metropolitan"
3 =     "Not identified"
;
value PEEDUCA
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
31 = "Less than 1st grade"
32 = "1st, 2nd, 3rd, or 4th grade"
33 = "5th or 6th grade"
34 = "7th or 8th grade"
35 = "9th grade"
36 = "10th grade"
37 = "11th grade"
38 = "12th grade - no diploma"
39 = "High school graduate - diploma or equivalent (GED)"
40 = "Some college but no degree"
41 = "Associate degree - occupational/vocational"
42 = "Associate degree - academic program"
43 = "Bachelor's degree (BA, AB, BS, etc.)"
44 = "Master's degree (MA, MS, MEng, MEd, MSW, etc.)"
45 = "Professional school degree (MD, DDS, DVM, etc.)"
46 = "Doctoral degree (PhD, EdD, etc.)"
;
value PEHSPNON
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Hispanic"
2 = "Non-Hispanic"
;
value PTDTRACE
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "White only"
2 = "Black only"
3 = "American Indian, Alaskan Native only"
4 = "Asian only"
5 = "Hawaiian/Pacific Islander only"
6 = "White-Black"
7 = "White-American Indian"
8 = "White-Asian"
9 = "White-Hawaiian"
10 = "Black-American Indian"
11 = "Black-Asian"
12 = "Black-Hawaiian"
13 = "American Indian-Asian"
14 = "Asian-Hawaiian"
15 = "White-Black-American Indian"
16 = "White-Black-Asian"
17 = "White-American Indian-Asian"
18 = "White-Asian-Hawaiian"
19 = "White-Black-American Indian-Asian"
20 = "2 or 3 races"
21 = "4 or 5 races"
;
value TELFS
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Employed - at work"
2 = "Employed - absent"
3 = "Unemployed - on layoff"
4 = "Unemployed - looking"
5 = "Not in labor force"
;
value TEMJOT
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value TESCHENR
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value TESCHLVL
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "High school"
2 = "College or university"
;
value TESPEMPNOT
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Employed"
2 = "Not employed"
;
value TRDPFTPT
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Full time"
2 = "Part time"
;
value TRHOLIDAY
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Diary day was not a holiday"
1 = "Diary day was a holiday"
;
value TRSPFTPT
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Full time"
2 = "Part time"
3 = "Hours vary"
;
value TRSPPRES
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Spouse present"
2 = "Unmarried partner present"
3 = "No spouse or unmarried partner present"
;
value TUDIARYDAY
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Sunday"
2 = "Monday"
3 = "Tuesday"
4 = "Wednesday"
5 = "Thursday"
6 = "Friday"
7 = "Saturday"
;
value TESEX
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Male"
2 = "Female"
;

proc contents data=atussum_0313; run;


/* Print out the first few records*/
proc print data=atussum_0313(obs=5);
   title2 'part of the raw data';
run;


/* Use Proc Tabulate to create a summary table */
proc tabulate data=titanic;
   title2 'Table of the number of survivors given passenger class and gender';
   class PClass Sex Survived;
   table PClass*Sex, Survived;
run;

proc tabulate data=titanic;
   title2 'Table of the proportion that survived given passenger class and gender';
   class PClass Sex;
   var Survived;
   table PClass*Sex, Survived*mean*f=7.3;
run;


/* Use the ODS system to extract the odds ratios */
proc freq data=titanic;
   by pclass;
   table sex*survived / relrisk;
   ods output RelativeRisks=myoddsratio;
run;

proc print data=myoddsratio;
   title2 'Odds ratio of survival for males vs females by passenger class';
run;


/* Use Proc SGplot to plot the odds ratio for each class */
proc sgplot data=myoddsratio noautolegend;
   title3 'Comparison of ODDS ratio (and 95% confidence intervals) among passenger classes';
   where studytype='Case-Control (Odds Ratio)';
   scatter y=Value x=PClass;
   highlow x=PClass low=LowerCL high=UpperCL;
   yaxis label="Odds ratio of SURVIVAL (male:female)";
   xaxis offsetmin=0.05 offsetmax=0.05 label='Passenger Class';
run;


ods pdf close;
