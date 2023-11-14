/***===========================================================================
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              第一步: 整理一带一路国家名单，与海关数据匹配
			  (1)与中国签订协议的国家 132个
			  (2)2013年一带一路沿线国家65个
_______________________________________________________________________________
===========================================================================***/
set more off
cd C:\papers\Belt

*** 转换stata版本数据——解决字符乱码问题
clear
 unicode encoding set gb18030
 unicode retranslate "iso3j_countrycode_countrynm.dta", transutf8
clear
 unicode encoding set gb18030
 unicode retranslate "一带一路与出口行为.do", transutf8
   
*** 与中国签订一带一路协议的国家
cd C:\papers\Belt
use "belt_cty.dta",clear  // 签订合作文件的国家数据
 merge 1:1 name_CH using "iso3j_countrycode_countrynm" 
  drop if _merge==2
  drop _merge
 sort country_code name_CH
 order name_CH country_code iso3j
 save "belt_cty.dta",replace
use "belt_cty.dta",clear
 keep name_CH country_code year month category
* drop if year==2019 & month>=5 // 马里 2019年7月签订，超过样本期
 egen group=group( category )
 label var group "1-合作；2-联合；3-谅解"
 drop name_CH category
 destring,replace
 rename (year month group)(B_year B_month B_group)
 save "belt_cty_merge",replace

*** 2013年公布的一带一路沿线65国
use "Belt65.dta" ,clear
 keep if belt==1
 gen year=2103
 drop name_CH
 rename belt belt65
 save "Belt65_merge",replace
 
*** 海上丝路国家整理 by陈万灵&何传添（2014）
use "belt_road",clear
 merge 1:1 name_CH using "iso3j_countrycode_countrynm"
  drop if _merge==2
  drop _merge
 keep country_code 
 gen sea_BR=1
 destring,replace
 compress
 save "belt_road_sea",replace



/***===========================================================================
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                             第二步: 平行趋势检验 
							         &DID回归结果
_______________________________________________________________________________
===========================================================================***/
cd C:\papers\Belt
set more off
/*================================================
            1.海关数据整理准备
================================================*/

****1.1 海关数据与一带一路国家匹配
/*海关数据为 产品-时间-目的国-城市-企业所有制-贸易国中转国-进出口海关口岸 
等层面的细分数据，加总到研究所需要的维度。 */

*基准回归结果数据
use "hg0718",clear
 bysort country_code hs8 year: egen export=total( Value )
 bysort country_code hs8 year: egen quantity=total( Quanti )
 drop Value Quanti
 duplicates drop country_code hs8 year,force
 
 merge m:1 country_code using "Belt65_merge.dta"
  drop if _merge==2
  drop _merge
 merge m:1 country_code using "belt_cty_merge.dta"
  drop if _merge==2
  drop _merge
 replace belt65=0 if belt65==.
 replace B_group=0 if B_group==. 
 save "hg07_18_BR_reg",replace  

****1.2 整理核心变量
use "hg07_18_BR_reg",clear   //国家3位数字代码

**1.2.1 被解释变量 
 gen price=export/quantity
 gen lnexport=ln(export)
 gen lnquantity=ln(quantity)
 gen lnprice=log(price)
 
**1.2.2 核心解释变量——乘积项
 gen treat1=(belt65==1)
 gen treat2=(B_group>=1)
 tab treat1
 tab treat2
 gen post1=(year>2012)
 gen post2=(year>=B_year)
 tab post1
 tab post2
 gen treat1_post1=treat1*post1 
 gen treat2_post2=treat2*post2
 
**1.2.3 平行趋势检验核心解释变量——treat*yeardummy 
* Belt65 单期DID
 tab year,gen(tdummy)
 forvalues i=1/12{
  gen treat1_d`i'=treat1*(tdummy`i')
  }
  
* belt_cty_132 多期DID
 replace B_year=2014 if B_year==. //对照组冲击时间设为B_year初始年份2014年
 gen ryear=year-B_year
 replace ryear=-6 if ryear<=-6
 tab ryear,gen(rtdummy)
 forvalues i=1/11{
  gen treat2_d`i'=treat2*(rtdummy`i')
  }
  
**1.2.4 匹配引力模型变量
 gen datayear=year
 merge m:1 country datayear using "gravity_vars2006_2017" 
  drop if _merge==2
  drop _merge  // _merge==1: 783,948
  
 save "hg07_18_BR_reg1",replace
 

/*================================================
                  2.DID回归结果
================================================*/  
******2.1 基准回归结果
use "hg07_18_BR_reg",clear
** 2.1.1 lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    //结果为正且显著
 est store a1
 
** 2.1.2 lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    //结果为正且显著
 est store a2
 
** 2.1.3 lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    //结果为负且显著
 est store a3
 
esttab  a1 a2 a3 /* 
        */using "Basic_Belt132_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 


 
/*================================================
               3.系数法平行趋势检验
================================================*/ 
cd C:\papers\Belt
set more off

****** 平行趋势检验
use "hg07_18_BR_reg1",clear
 drop treat1_d1 treat2_d1
**** 3.1 lnexport
reghdfe lnexport treat2_d* rta pergdp totpop,absorb(year country_code) cluster(country_code)
 est store x1 
coefplot x1, keep(treat2_d*) vertical recast(line) xline(5,lp(dash)) ///
 xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "+1" 8 "+2" 9 "+3" 10 "+4")   ///
 level(95) ylabel(-0.3(0.3)0.6) xtitle("ryear") ytitle("Coef. of treat_ryeardummy") ///
 graphregion(color(white)) ylabel(, format(%2.1f))
 gr save "Belt132_1_1_export.gph",replace 
 
**** 3.2 lnquantity
reghdfe lnquantity treat2_d* rta pergdp totpop,absorb(year country_code) cluster(country_code)
 est store x2 
coefplot x2, keep(treat2_d*) vertical recast(line) xline(5,lp(dash)) ///
 xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "+1" 8 "+2" 9 "+3" 10 "+4")   ///
 level(95) ylabel(-0.3(0.3)0.6) xtitle("ryear") ytitle("Coef. of treat_ryeardummy") ///
 graphregion(color(white)) ylabel(, format(%2.1f))
 gr save "Belt132_1_2_quantity.gph",replace 
 
**** 3.3 lnprice
reghdfe lnprice treat2_d* rta pergdp totpop,absorb(year country_code) cluster(country_code)
 est store x3 
coefplot x3, keep(treat2_d*) vertical recast(line) xline(5,lp(dash)) ///
 xlabel(1 "-5" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "0" 7 "+1" 8 "+2" 9 "+3" 10 "+4")   ///
 level(95) ylabel(-0.4(0.1)0.1) xtitle("ryear") ytitle("Coef. of treat_ryeardummy") ///
 graphregion(color(white)) ylabel(, format(%2.1f))
 gr save "Belt132_1_3_price.gph",replace 

 save "hg07_18_BR_reg_basic",replace
 
 
/*================================================
          4.识别条件检验-预期效应
================================================*/ 
use "hg07_18_BR_reg1",clear
 drop treat1_d* treat2_d*
 gen ryear_1=1 if ryear==-1
 replace ryear_1=0 if ryear_1==.
 gen treat_ryear_1=treat2*ryear_1

**** 4.1 lnexport
reghdfe lnexport treat2_post2 treat_ryear_1 rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)
 est store a1
**** 4.2 lnquantity
reghdfe lnquantity treat2_post2 treat_ryear_1 rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code) 
 est store a2
**** 4.3 lnprice
reghdfe lnprice treat2_post2 treat_ryear_1 rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
esttab  a1 a2 a3 /* 
        */using "Specification_1_expect_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2 treat_ryear_1) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 


/*================================================
          5.识别条件检验-安慰剂检验
================================================*/ 
cd C:\papers\Belt
set more off
use "hg07_18_BR_reg1",clear
 drop citycode hs8 belt65 B_month B_group export quantity ///
  price treat1 post1 treat1_post1 treat2_post2 datayear /// 
  ryear iso3j gdpj rgdppcj countrynm dist comlangoff ///
  treat1_d* treat2_d* tdummy* rtdummy*
 compress
 replace post2=1 if year>=2014 & treat2==0
 drop B_year treat2
 save "hg07_18_BR_placebo",replace
 
******placebo test********
** 5.1 随机选取控制组行业 和 冲击时间
set more off
use "hg07_18_BR_placebo",clear
keep country_code
duplicates drop country_code,force
save "belt_treat_placebo",replace
*
set seed 7
forv i=1/500{
use "belt_treat_placebo", clear
*控制组随机
 gen u= runiform()
  sort u
  egen id=seq()
  gen faketreat=(id<=132)
  drop u id
*冲击时间随机
 gen u= runiform()
  sort u
  egen id=seq()
  gen fakeyear=2008 if id<=24*(2008-2007)
  /* 240个国家，10年，故每年有24个国家 */
  forvalues j=2009/2017{
   replace fakeyear=`j' if id>24*(`j'-2008) & id<=24*(`j'-2007)
   }
keep country_code faketreat fakeyear
sort country_code
save "belt_treat_placebo`i'", replace
}

** 5.2 fake regression
forv i=1/500{

use "hg07_18_BR_placebo",clear
 cap drop post treat
****fake treatment dummy
 merge m:1 country_code using "belt_treat_placebo`i'.dta"
  drop if _merge==2
  drop _merge
 gen fakepost=(year>=fakeyear)

 gen treat_post_placebo= faketreat*fakepost

**** lnexport
qui reghdfe lnexport treat_post_placebo rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    
gen exp_placebo=_b[treat_post_placebo]
gen expse_placebo=_se[treat_post_placebo]

**** lnquantity
qui reghdfe lnquantity treat_post_placebo rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code) 
gen quant_placebo=_b[treat_post_placebo]
gen quantse_placebo=_se[treat_post_placebo]

**** lnprice
qui reghdfe lnprice treat_post_placebo rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code) 
gen price_placebo=_b[treat_post_placebo]
gen pricese_placebo=_se[treat_post_placebo]
keep in 1
keep exp_placebo expse_placebo  quant_placebo quantse_placebo price_placebo pricese_placebo 

gen id=`i'
compress
save "result_placebo_`i'", replace
}
*
 
use result_placebo_1, clear
forv i=2/500{
append using "result_placebo_`i'.dta"
}
save "result_placebo_belt_500", replace

forv i=1/500{
erase "result_placebo_`i'.dta"
erase "belt_treat_placebo`i'.dta"
}

**************************
** 5.3 Graph
use "result_placebo_belt_500", clear
sum exp_placebo quant_placebo price_placebo

gen export_value=exp_placebo
gen export_quantity=quant_placebo
gen export_price=price_placebo

twoway (kdensity export_value) ///
 , xline(0.0872, lp(shortdash) lc(black)) /// 
 xlabel(-0.09(0.03)-0.03 0 0.03(0.03)0.09) legend(off) ///
 xlabel(, format(%3.2f)) graphregion(color(white)) /// 
 xtitle("Coef. of treat_post_placebo") saving(placebo_exp,replace) 

twoway (kdensity export_quantity) ///
 , xline(0.1343, lp(shortdash) lc(black)) /// 
 xlabel(-0.12(0.04)0.16) legend(off) ///
 xlabel(, format(%3.2f)) graphregion(color(white)) /// 
 xtitle("Coef. of treat_post_placebo") saving(placebo_quant,replace) 
 
twoway (kdensity export_price) ///
 , xline(-0.0471, lp(shortdash) lc(black)) /// 
 xlabel(-0.06(0.02)0.06) xtitle("") legend(off) ///
 xlabel(, format(%3.2f)) graphregion(color(white)) /// 
 xtitle("Coef. of treat_post_placebo") saving(placebo_price,replace) 
 



/***===========================================================================
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                             第三步: 稳健性分析 
_______________________________________________________________________________
===========================================================================***/
cd C:\papers\Belt
set more off
/*================================================
            1.一带一路沿线国家Belt65
================================================*/

use "hg07_18_BR_reg_basic",clear
**** 1.1 lnexport
reghdfe lnexport treat1_post1 rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    //结果为正且显著
 est store a6
**** 1.2 lnquantity
reghdfe lnquantity treat1_post1 rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    //结果为正且显著
 est store a7
**** 1.3 lnprice
reghdfe lnprice treat1_post1 rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    //结果为负且显著
 est store a8
esttab  a6 a7 a8 /* 
        */using "Robust1_Belt65_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat1_post1) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 


**** 1.4 比较 belt65 和belt132 估计系数的差异是否显著
capture program drop rd_boot
program rd_boot, eclass
	tempvar touse
	gen byte `touse' = 1
	qui reghdfe lnexport treat1_post1 rta pergdp totpop, /// 
                absorb(year country_code) cluster(country_code)    
	scalar rd_b = _b[treat1_post1]		
	qui reghdfe lnexport treat2_post2 rta pergdp totpop, /// 
                absorb(year country_code) cluster(country_code)  
	scalar rd_a = _b[treat2_post2]
	scalar diff = rd_a - rd_b
		
	matrix b = (diff)
	matrix colnames b = diff
	ereturn post b , esample(`touse')
	ereturn display
end

rd_boot
bootstrap _b[diff], reps(10) seed(7): rd_boot 


/*================================================
            2.post设置
================================================*/
use "hg07_18_BR_reg_basic",clear
* gen post1=(year>2012)
* gen post2=(year>=B_year)
 gen post_robust=post2
 replace post_robust=(12-B_month)/12 if year==B_year
 gen treat_postr=treat2*post_robust
 
**** 2.1 lnexport
reghdfe lnexport treat_postr rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    //结果为正且显著
 est store a6
**** 2.2 lnquantity
reghdfe lnquantity treat_postr rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    //结果为正且显著
 est store a7
**** 2.3 lnprice
reghdfe lnprice treat_postr rta pergdp totpop, /// 
 absorb(year country_code) cluster(country_code)    //结果为负且显著
 est store a8
esttab  a6 a7 a8 /* 
        */using "Robust2_Belt132_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat_postr) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 


capture program drop rd_boot
program rd_boot, eclass
	tempvar touse
	gen byte `touse' = 1
	qui reghdfe lnexport treat_postr rta pergdp totpop, /// 
                absorb(year country_code) cluster(country_code)    
	scalar rd_b = _b[treat_postr]		
	qui reghdfe lnexport treat2_post2 rta pergdp totpop, /// 
                absorb(year country_code) cluster(country_code)  
	scalar rd_a = _b[treat2_post2]
	scalar diff = rd_a - rd_b
		
	matrix b = (diff)
	matrix colnames b = diff
	ereturn post b , esample(`touse')
	ereturn display
end

rd_boot
bootstrap _b[diff], reps(10) seed(7): rd_boot 

/*================================================
            3.目的国进口关税及产品固定效应
================================================*/
use "hg07_18_BR_reg1",clear
 drop treat1_d* treat2_d*
 egen cty_hs8=group(country_code hs8)
 egen hs8_year=group(year hs8)

**** 3.1 lnexport
reghdfe lnexport treat2_post2  rta pergdp totpop, /// 
 absorb(year country_code hs8) cluster(country_code)
 est store a1
**** 3.2 lnquantity
reghdfe lnquantity treat2_post2  rta pergdp totpop, /// 
 absorb(year country_code hs8) cluster(country_code) 
 est store a2
**** 3.3 lnprice
reghdfe lnprice treat2_post2  rta pergdp totpop, /// 
 absorb(year country_code hs8) cluster(country_code) 
 est store a3
 
**** 3.4 lnexport
reghdfe lnexport treat2_post2  rta pergdp totpop, /// 
 absorb(year cty_hs8) cluster(country_code)
 est store a4
**** 3.5 lnquantity
reghdfe lnquantity treat2_post2  rta pergdp totpop, /// 
 absorb(year cty_hs8) cluster(country_code) 
 est store a5
**** 3.6 lnprice
reghdfe lnprice treat2_post2  rta pergdp totpop, /// 
 absorb(year cty_hs8) cluster(country_code) 
 est store a6
esttab  a1 a2 a3 a4 a5 a6 /* 
        */using "Robust3_HS8fixed_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 
  
**** 3.7 lnexport
reghdfe lnexport treat2_post2  rta pergdp totpop, /// 
 absorb(year cty_hs8 hs8_year) cluster(country_code)
 est store a7
**** 3.8 lnquantity
reghdfe lnquantity treat2_post2  rta pergdp totpop, /// 
 absorb(year cty_hs8 hs8_year) cluster(country_code) 
 est store a8
**** 3.9 lnprice
reghdfe lnprice treat2_post2  rta pergdp totpop, /// 
 absorb(year cty_hs8 hs8_year) cluster(country_code) 
 est store a9
 
esttab  a1 a2 a3 a4 a5 a6 a7 a8 a9 /* 
        */using "Robust3_HS8fixed_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 
 
 
 
 
/***===========================================================================
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                             第三步: 异质性分析 
_______________________________________________________________________________
===========================================================================***/
/*================================================
            1. 一带 与 一路
================================================*/

use "hg07_18_BR_reg_basic",clear
 merge m:1 country_code using "belt_road_sea"
  drop _merge
 replace sea_BR=0 if sea_BR==.  //123
 tab sea_BR
/*
     sea_BR |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |  4,522,470       78.17       78.17
          1 |  1,262,961       21.83      100.00
------------+-----------------------------------
      Total |  5,785,431      100.00
*/

**** lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop if sea_BR==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if sea_BR==1 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
**** lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop if sea_BR==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
reghdfe lnquantity treat2_post2 rta pergdp totpop if sea_BR==1 , /// 
 absorb(year country_code) cluster(country_code)
 est store a4
**** lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop if sea_BR==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a5
reghdfe lnprice treat2_post2 rta pergdp totpop if sea_BR==1 , /// 
 absorb(year country_code) cluster(country_code)
 est store a6
 
esttab  a1 a2 a3 a4 a5 a6 /* 
        */using "Hetero1_1_Belt132_sea_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 


**** lnexport
reghdfe lnexport treat1_post1 rta pergdp totpop if sea_BR==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat1_post1 rta pergdp totpop if sea_BR==1 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
**** lnquantity
reghdfe lnquantity treat1_post1 rta pergdp totpop if sea_BR==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
reghdfe lnquantity treat1_post1 rta pergdp totpop if sea_BR==1 , /// 
 absorb(year country_code) cluster(country_code)
 est store a4
**** lnprice
reghdfe lnprice treat1_post1 rta pergdp totpop if sea_BR==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a5
reghdfe lnprice treat1_post1 rta pergdp totpop if sea_BR==1 , /// 
 absorb(year country_code) cluster(country_code)
 est store a6
 
esttab  a1 a2 a3 a4 a5 a6 /* 
        */using "Hetero1_2_Belt65_sea_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat1_post1) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 

/*================================================
            2.签约种类异质性
================================================*/

use "hg07_18_BR_reg_basic",clear
**** 2.1 lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop if B_group==3|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if B_group==1|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a2
reghdfe lnexport treat2_post2 rta pergdp totpop if B_group==2|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3

**** 2.2 lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop if B_group==3|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a4
reghdfe lnquantity treat2_post2 rta pergdp totpop if B_group==1|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a5
reghdfe lnquantity treat2_post2 rta pergdp totpop if B_group==2|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a6
 
**** 2.3 lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop if B_group==3|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a7
reghdfe lnprice treat2_post2 rta pergdp totpop if B_group==1|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a8
reghdfe lnprice treat2_post2 rta pergdp totpop if B_group==2|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a9

esttab  a1 a2 a3 a4 a5 a6 a7 a8 a9 /* 
        */using "Hetero2_Belt132_group_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 

 
** 1-合作，2-联合，3-谅解
**** 2.1 lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop if B_group==1|B_group==3|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if B_group==2|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a2

**** 2.2 lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop if B_group==1|B_group==3|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
reghdfe lnquantity treat2_post2 rta pergdp totpop if B_group==2|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a4
 
**** 2.3 lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop if B_group==1|B_group==3|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a5
reghdfe lnprice treat2_post2 rta pergdp totpop if B_group==2|B_group==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a6

esttab  a1 a2 a3 a4 a5 a6 /* 
        */using "Hetero2_2_Belt132_group_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 
		
		
		
/*================================================
        3.一带一路沿线城市：DDD
================================================*/

clear
 unicode encoding set gb18030
 unicode retranslate "city_vars_06_17_forreg1.dta", transutf8
 
cd C:\papers\Belt\jiang
clear
 unicode encoding set gb18030
 unicode retranslate "BRcity.dta", transutf8

cd C:\papers\Belt
****3.1 一带一路城市整理
**3.1.1 整理城市代码
use "city_vars_06_17_forreg1",clear
 keep citynm citycode province
 duplicates drop citycode citynm,force
 tostring citycode,replace
 gen procode=substr(citycode,1,2)
 destring procode citycode,replace
 save "city_pro_code",replace 
**3.1.2 愿景
use "br_yuanjing_pro",clear
 duplicates drop
 save,replace
use "br_yuanjing_city",clear
 duplicates drop
 save,replace
 
use "city_pro_code",clear 
 merge m:1 province using "br_yuanjing_pro"
  gen belt_city=1 if _merge==3  //123
  drop _merge
 merge m:1 citynm using "br_yuanjing_city"
  replace belt_city=1 if _merge==3 & belt_city==. //12
 replace belt_city=1 if citynm=="合肥市" 
  replace citycode=3401  if citynm=="合肥市"
 replace belt_city=1 if citynm=="平潭市" 
  replace citycode=3501  if citynm=="平潭市" //福州市平潭县
 **港澳台
 keep citycode belt_city
 drop if belt_city!=1 
 duplicates drop // 福州市因平潭县而重复
 rename belt_city belt_yuanjing
 save "brcity_yuanjing",replace  //136
 
**3.1.3 线路
use "br_city_gf",clear
 keep citynm_xl
 drop if citynm_xl==""
 duplicates drop
 rename citynm_xl citynm
 save "br_xianlu_city",replace //63

use "city_pro_code",clear 
 merge m:1 citynm using "br_xianlu_city"
  gen belt_city=1 if _merge==3  //52
  drop _merge
 replace belt_city=1 if citynm=="合肥市" 
  replace citycode=3401  if citynm=="合肥市" 
 replace belt_city=1 if citynm=="伊犁哈萨克自治州" 
  replace citycode=6540  if citynm=="伊犁哈萨克自治州" 
 replace belt_city=1 if citynm=="吐鲁番市" 
  replace citycode=6504  if citynm=="吐鲁番市" 
 replace belt_city=1 if citynm=="哈密市" 
  replace citycode=6522  if citynm=="哈密市" 
 replace belt_city=1 if citynm=="喀什市" 
  replace citycode=6531  if citynm=="喀什市" 
 replace belt_city=1 if citynm=="大理白族自治州" 
  replace citycode=5329  if citynm=="大理白族自治州" 
 replace belt_city=1 if citynm=="德宏傣族景颇族自治州" 
  replace citycode=5331  if citynm=="德宏傣族景颇族自治州" 
 replace belt_city=1 if citynm=="敦煌市" 
  replace citycode=6209  if citynm=="敦煌市"  //酒泉市
 replace belt_city=1 if citynm=="玉门市" 
  replace citycode=6209  if citynm=="玉门市"  //酒泉市
 **香港 澳门
 keep citycode belt_city
 drop if belt_city!=1 
 duplicates drop // 福州市因平潭县而重复
 rename belt_city  belt_xianlu
 save "brcity_xianlu",replace  //59

**3.1.4 百度
use "br_city_gf",clear
 keep citynm_bd
 drop if citynm_bd==""
 duplicates drop
 rename citynm_bd province
 save "br_baidu_pro",replace //18省、直辖市
 
use "city_pro_code",clear 
 merge m:1 province using "br_baidu_pro"
  gen belt_city=1 if _merge==3  //142
  drop _merge
 keep citycode belt_city
 drop if belt_city!=1 
 duplicates drop // 福州市因平潭县而重复
 rename belt_city  belt_baidu
 save "brcity_baidu",replace  //142


use "city_pro_code",clear 
 merge m:1 province using "district"
  drop _merge
 keep citycode district
 replace district=0 if district==.
 label var district "0-east;1-middle;2-west"
 save "br_district",replace  //142

 
****3.1.5 匹配一带一路沿线城市
use "hg07_18",clear
 bysort citycode country_code hs8 year: egen export=total( Value )
 bysort citycode country_code hs8 year: egen quantity=total( Quanti )
 drop Value Quanti
 duplicates drop citycode country_code hs8 year,force
 merge m:1 country_code using "Belt65_merge.dta"
  drop if _merge==2
  drop _merge
 merge m:1 country_code using "belt_cty_merge.dta"
  drop if _merge==2
  drop _merge
 replace belt65=0 if belt65==.
 replace B_group=0 if B_group==. 
**被解释变量 
 gen price=export/quantity
 gen lnexport=ln(export)
 gen lnquantity=ln(quantity)
 gen lnprice=log(price)
**核心解释变量——乘积项
 gen treat1=(belt65==1)
 gen treat2=(B_group>=1)
 tab treat1
 tab treat2
 gen post1=(year>2012)
 gen post2=(year>=B_year)
 tab post1
 tab post2
 gen treat1_post1=treat1*post1 
 gen treat2_post2=treat2*post2

 merge m:1 citycode using "brcity_yuanjing"
  drop if _merge==2
  drop _merge
 merge m:1 citycode using "brcity_xianlu"
  drop if _merge==2
  drop _merge 
 merge m:1 citycode using "brcity_baidu"
  drop if _merge==2
  drop _merge 

 for any belt_yuanjing belt_xianlu belt_baidu:replace X=0 if X==.
 *belt_yuanjing belt_xianlu belt_baidu
 gen brcity_treat_post=brcity*treat2_post2
 gen brcityyj_treat_post=belt_yuanjing*treat2_post2
 gen brcityxl_treat_post=belt_xianlu*treat2_post2
 gen brcitybd_treat_post=belt_baidu*treat2_post2
 
 gen brcity_treat=brcity*treat2
 gen brcity_post=brcity*post2
 gen brcityyj_treat=belt_yuanjing*treat2
 gen brcityyj_post=belt_yuanjing*post2 
 gen brcityxl_treat=belt_xianlu*treat2
 gen brcityxl_post=belt_xianlu*post2 
 gen brcitybd_treat=belt_baidu*treat2
 gen brcitybd_post=belt_baidu*post2 

 compress
 drop tdummy1- treat2_d11 datayear countrynm _est_x1 _est_x2 _est_x3 /// 
  fproduct_num firm_num product_num lnfirm_num lnfprod_num lnprod_num /// 
  citynm province _est_a1- _est_a6
 save "hg07_18_BR_reg_brcity",replace
 
**** 3.3 regressions
use "hg07_18_BR_reg_brcity",clear
 merge m:1 citycode using "br_district"
  drop if _merge==2
  drop _merge
 gen midwest=1 if district==1|district==2
  replace midwest=0 if district==0
 gen midwest_treat_post=midwest*treat2_post2 
 gen midwest_treat=midwest*treat2
 gen midwest_post=midwest*post2

 global controlvars rta pergdp totpop lnGDP lnstudent lnfeishui /// 
  seacity lnunemp_rate lnkeyun_road lntotbook lnsale lnFDI lnperGDP

reghdfe lnexport treat2_post2 $controlvars if midwest==1, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a4_0
reghdfe lnexport treat2_post2 $controlvars if midwest==0, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a5_0

reghdfe lnquantity treat2_post2 $controlvars if midwest==1, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a4_1
reghdfe lnquantity treat2_post2 $controlvars if midwest==0, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a5_1
 
reghdfe lnprice treat2_post2 $controlvars if midwest==1, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a4_2
reghdfe lnprice treat2_post2 $controlvars if midwest==0, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a5_2
esttab a4_0 a5_0 a4_1 a5_1 a4_2 a5_2  /* 
        */using "Hetero3_west_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop( $controlvars _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 

capture program drop rd_boot
program rd_boot, eclass
	tempvar touse
	gen byte `touse' = 1
	qui reghdfe lnexport treat2_post2 $controlvars if belt_yuanjing==1, /// 
                absorb(citycode year country_code) cluster(country_code)    
	scalar rd_b = _b[treat2_post2]		
	qui reghdfe lnexport treat2_post2 $controlvars if belt_yuanjing==0, /// 
                absorb(citycode year country_code) cluster(country_code)  
	scalar rd_a = _b[treat2_post2]
	scalar diff = rd_a - rd_b
		
	matrix b = (diff)
	matrix colnames b = diff
	ereturn post b , esample(`touse')
	ereturn display
end

rd_boot
bootstrap _b[diff], reps(10) seed(7): rd_boot 

/*

------------------------------------------------------------------------------
             |   Observed   Bootstrap                         Normal-based
             |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       _bs_1 |  -.0433024    .007848    -5.52   0.000    -.0586843   -.0279205
------------------------------------------------------------------------------

*/
reghdfe lnexport treat2_post2 $controlvars if belt_yuanjing==1, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a8
reghdfe lnexport treat2_post2 $controlvars if belt_yuanjing==0, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a9

reghdfe lnquantity treat2_post2 $controlvars if belt_yuanjing==1, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a8_1
reghdfe lnquantity treat2_post2 $controlvars if belt_yuanjing==0, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a9_1
 
reghdfe lnprice treat2_post2 $controlvars if belt_yuanjing==1, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a8_2
reghdfe lnprice treat2_post2 $controlvars if belt_yuanjing==0, /// 
 absorb(citycode year country_code) cluster(country_code) 
 est store a9_2
esttab a8 a9 a8_1 a9_1 a8_2 a9_2  /* 
        */using "Hetero3_yuanjing_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop( $controlvars _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 
 

 
/*================================================
        4.贸易方式：一般贸易vs加工贸易
================================================*/
cd C:\papers\Belt
set more off

clear
 unicode encoding set gb18030
 unicode retranslate "hg07-18_export_trademode.dta", transutf8

****4.1 加总得到不同贸易方式的出口额 出口数量 出口价格
use "hg07-18_export_trademode",clear
 compress
 drop _impexptypecd citycode _begendcntrynm _trdmdnm _quantityunitnm prov
 rename (_begendcntrycd hs_hscd _year)(country_code hs8 year)
 compress
 bysort country_code hs8 year trade_mode: egen export=total( V )
 bysort country_code hs8 year trade_mode: egen quantity=total( Q )
 drop V Q
 duplicates drop country_code hs8 year trade_mode,force
 gen price=export/quantity
****4.2 被解释变量 
 gen lnexport=ln(export)
 gen lnquantity=ln(quantity)
 gen lnprice=log(price) 
****4.3 匹配一带一路国家变量  
 merge m:1 country_code using "Belt65_merge.dta"
  drop if _merge==2
  drop _merge
 merge m:1 country_code using "belt_cty_merge.dta"
  drop if _merge==2
  drop _merge
 replace belt65=0 if belt65==.
 replace B_group=0 if B_group==. 
****4.4 核心解释变量——乘积项
 gen treat1=(belt65==1)
 gen treat2=(B_group>=1)
 tab treat1
 tab treat2
 gen post1=(year>2012)
 gen post2=(year>=B_year)
 tab post1
 tab post2
 gen treat1_post1=treat1*post1 
 gen treat2_post2=treat2*post2
****4.5 匹配引力模型变量
 gen datayear=year
 merge m:1 country datayear using "gravity_vars2006_2017" 
  drop if _merge==2
  drop _merge  // _merge==1: 1,090,586
 compress
 save "hg07_18_BR_reg_trademode",replace

****4.6 回归结果
use "hg07_18_BR_reg_trademode",clear
 drop texport tquantity dist comlangoff iso3j gdpj rgdppcj ///
   countrynm treat1 post1 treat1_post1  
   
 bysort country_code hs8 year : egen texport=total(export)
 bysort country_code hs8 year : egen tquantity=total(quantity)
 gen export_share=export/texport
 gen quantity_share=quantity/tquantity 

 gen ordinary=1 if trade_mode==1|trade_mode==3
  replace ordinary=0 if trade_mode==2
 bysort country_code hs8 year ordinary:egen oexport=total(export)
 bysort country_code hs8 year ordinary:egen oquantity=total(quantity)
 bysort country_code hs8 year ordinary:egen oexport_share=total(export_share)
 bysort country_code hs8 year ordinary:egen oquantity_share=total(quantity_share)
 
**== 第一类：将一般贸易、加工贸易、其他分别估计
**** lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop if trade_mode==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if trade_mode==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
reghdfe lnexport treat2_post2 rta pergdp totpop if trade_mode==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
**** lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop if trade_mode==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a4
reghdfe lnquantity treat2_post2 rta pergdp totpop if trade_mode==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a5
reghdfe lnquantity treat2_post2 rta pergdp totpop if trade_mode==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a6
**** lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop if trade_mode==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a7
reghdfe lnprice treat2_post2 rta pergdp totpop if trade_mode==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a8
reghdfe lnprice treat2_post2 rta pergdp totpop if trade_mode==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a9
esttab  a1 a2 a3 a4 a5 a6 a7 a8 a9 /* 
        */using "Hetero4_1_Belt132_mode_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 


**** export share 
 drop belt65 B_year B_month B_group
 compress
 duplicates drop country_code hs8 year ordinary,force 
 save "hg07_18_BR_reg_trademode_share",replace
 
use "hg07_18_BR_reg_trademode_share",clear
 for any oexport_share oquantity_share: replace X=X*100
 
reghdfe oexport_share treat2_post2 rta pergdp totpop if ordinary==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe oexport_share treat2_post2 rta pergdp totpop if ordinary==0 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
**** lnquantity
reghdfe oquantity_share treat2_post2 rta pergdp totpop if ordinary==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
reghdfe oquantity_share treat2_post2 rta pergdp totpop if ordinary==0 , /// 
 absorb(year country_code) cluster(country_code)
 est store a4
esttab  a1 a2 a3 a4 /* 
        */using "Hetero4_3_Belt132_modeshare_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 


**== 第二类：一般贸易及其他、加工贸易分别估计
**** lnexport
gen oprice=oexport/oquantity
for any oexport oquantity oprice:gen lnX=log(X)

reghdfe lnoexport treat2_post2 rta pergdp totpop if ordinary==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnoexport treat2_post2 rta pergdp totpop if ordinary==0 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
**** lnquantity
reghdfe lnoquantity treat2_post2 rta pergdp totpop if ordinary==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
reghdfe lnoquantity treat2_post2 rta pergdp totpop if ordinary==0 , /// 
 absorb(year country_code) cluster(country_code)
 est store a4
**** lnprice
reghdfe lnoprice treat2_post2 rta pergdp totpop if ordinary==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a5
reghdfe lnoprice treat2_post2 rta pergdp totpop if ordinary==0 , /// 
 absorb(year country_code) cluster(country_code)
 est store a6
 
esttab  a1 a2 a3 a4 a5 a6 /* 
        */using "Hetero4_2_Belt132_modeordi_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 


capture program drop rd_boot
program rd_boot, eclass
	tempvar touse
	gen byte `touse' = 1
 
	qui reghdfe lnexport treat2_post2 rta pergdp totpop if trade_mode==1, /// 
                absorb(year country_code) cluster(country_code)   
	scalar rd_b = _b[treat2_post2]		
    qui reghdfe lnexport treat2_post2 rta pergdp totpop if trade_mode==2, /// 
                absorb(year country_code) cluster(country_code)   
	scalar rd_a = _b[treat2_post2]
	scalar diff = rd_a - rd_b
		
	matrix b = (diff)
	matrix colnames b = diff
	ereturn post b , esample(`touse')
	ereturn display
end

rd_boot
bootstrap _b[diff], reps(10) seed(7): rd_boot 


use "hg07_18_BR_reg_trademode_share",clear
 keep if year==2018
 bysort treat2 ordinary:egen o_export=total(export)
 bysort treat2 :egen t_export=total(export)
 gen share=o_export/t_export
 su share if ordinary==1&treat2==1
 su share if ordinary==0&treat2==1


/*================================================
       5.贸易货物类型：货物贸易vs服务贸易
================================================*/

cd C:\papers\Belt
set more off

****5.1 划分农业、制造业、服务业行业
use "hg07_18_DD",clear
 gen hs6=substr(hs_hscd,1,6)
 merge m:1 hs6 using "hs6-sitcr2.dta"
  drop if _merge==2
  drop _merge
 destring sitcr2_4,replace
 gen industry=2 if sitcr2_4<9000 & sitcr2_4>5000   //制造业
  replace industry=3 if sitcr2_4>9000 & sitcr2_4!=.   //服务业
  replace industry=1 if sitcr2_4<5000   //农业

 rename (_begendcntrycd hs_hscd _year)(country_code hs8 year)
 bysort country_code hs8 year industry: egen export=total( totV )
 bysort country_code hs8 year industry: egen quantity=total( totQ )
 drop totV totQ
 duplicates drop country_code hs8 year industry,force
 gen price=export/quantity
****5.2 被解释变量 
 gen lnexport=ln(export)
 gen lnquantity=ln(quantity)
 gen lnprice=log(price) 
****5.3 匹配一带一路国家变量  
 merge m:1 country_code using "Belt65_merge.dta"
  drop if _merge==2
  drop _merge
 merge m:1 country_code using "belt_cty_merge.dta"
  drop if _merge==2
  drop _merge
 replace belt65=0 if belt65==.
 replace B_group=0 if B_group==. 
****5.4 核心解释变量——乘积项
 gen treat1=(belt65==1)
 gen treat2=(B_group>=1)
 tab treat1
 tab treat2
 gen post1=(year>2012)
 gen post2=(year>=B_year)
 tab post1
 tab post2
 gen treat1_post1=treat1*post1 
 gen treat2_post2=treat2*post2

****5.5 匹配引力模型变量
 gen datayear=year
 merge m:1 country datayear using "gravity_vars2006_2017" 
  drop if _merge==2
  drop _merge  // _merge==1: 1,090,586
 compress
 save "hg07_18_BR_reg_industry",replace

****5.6 回归结果
use "hg07_18_BR_reg_industry",clear
**== 第一类：农业、制造业、服务业分别估计
**** lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop if industry==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if industry==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
reghdfe lnexport treat2_post2 rta pergdp totpop if industry==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
**** lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop if industry==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a4
reghdfe lnquantity treat2_post2 rta pergdp totpop if industry==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a5
reghdfe lnquantity treat2_post2 rta pergdp totpop if industry==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a6
**** lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop if industry==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a7
reghdfe lnprice treat2_post2 rta pergdp totpop if industry==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a8
reghdfe lnprice treat2_post2 rta pergdp totpop if industry==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a9
esttab  a1 a2 a3 a4 a5 a6 a7 a8 a9 /* 
??????  */using "Hetero5_1_Belt132_industry_DID.rtf", replace ar2(%9.4f) /* 
??????  */b(%9.4f) se(%9.4f) nogap  /* 
??????  */drop(rta pergdp totpop _cons) /* 
??????  */order(treat2_post2) /* 
??????  */ star(* 0.10  **  0.05  *** 0.01) /* 
??????  */title("Belt") scalar(N) 

**== 第二类：农业、制造业为货物贸易，与服务贸易比较
**** lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop if industry==1|industry==2, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if industry==3 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
**** lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop if industry==1|industry==2, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
reghdfe lnquantity treat2_post2 rta pergdp totpop if industry==3 , /// 
 absorb(year country_code) cluster(country_code)
 est store a4
**** lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop if industry==1|industry==2, /// 
 absorb(year country_code) cluster(country_code) 
 est store a5
reghdfe lnprice treat2_post2 rta pergdp totpop if industry==3 , /// 
 absorb(year country_code) cluster(country_code)
 est store a6
 
esttab  a1 a2 a3 a4 a5 a6 /* 
        */using "Hetero5_2_Belt132_industry_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 




/*================================================
        6.贸易产品种类：中间品、资本品
================================================*/

cd C:\papers\Belt
set more off

use "hg07_18_BR_reg_basic",clear
 gen hs6=substr(hs8,1,6)
*产品用途分类：BEC "1 中间品 2 资本品 3 消费品"
 merge m:1 hs6 using "HS02_BEC.dta"
  drop if _merge==2
  drop _merge
 destring bec, force replace
 gen BEC=1 if bec==111 | bec==121 | bec==21 | bec==22 | bec==31 | bec==322 | bec==42 | bec==53
  replace BEC=2 if bec==41 | bec==521 									   
  replace BEC=3 if bec==112 | bec==122 | bec==522 | bec==61 | bec==62 | bec==63

**==1 中间品 2 资本品 3 消费品 分别估计
/*
        BEC |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |  2,837,238       55.65       55.65
          2 |    989,018       19.40       75.04
          3 |  1,272,379       24.96      100.00
------------+-----------------------------------
      Total |  5,098,635      100.00
*/
**** lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop if BEC==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if BEC==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
reghdfe lnexport treat2_post2 rta pergdp totpop if BEC==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
**** lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop if BEC==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a4
reghdfe lnquantity treat2_post2 rta pergdp totpop if BEC==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a5
reghdfe lnquantity treat2_post2 rta pergdp totpop if BEC==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a6
**** lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop if BEC==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a7
reghdfe lnprice treat2_post2 rta pergdp totpop if BEC==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a8
reghdfe lnprice treat2_post2 rta pergdp totpop if BEC==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a9
esttab  a1 a2 a3 a4 a5 a6 a7 a8 a9 /* 
        */using "Hetero6_1_Belt132_BEC_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 


capture program drop rd_boot
program rd_boot, eclass
	tempvar touse
	gen byte `touse' = 1
 
	qui reghdfe lnexport treat2_post2 rta pergdp totpop if BEC==1, /// 
                absorb(year country_code) cluster(country_code)    
	scalar rd_b = _b[treat2_post2]		
    qui reghdfe lnexport treat2_post2 rta pergdp totpop if BEC==2, /// 
                absorb(year country_code) cluster(country_code)    
	scalar rd_a = _b[treat2_post2]
	scalar diff = rd_a - rd_b
		
	matrix b = (diff)
	matrix colnames b = diff
	ereturn post b , esample(`touse')
	ereturn display
end

rd_boot
bootstrap _b[diff], reps(10) seed(7): rd_boot 

/*================================================
        7.Rauch1999-同质产品与异质产品
================================================*/

cd C:\papers\Belt
set more off

use "hg07_18_BR_reg_basic",clear
 gen hs6=substr(hs8,1,6)

*(5)保留垂直差异化产品，剔除同质商品
*同异质产品分类：n "异质产品, w、r 同质产品"  Rauch(1999)
merge m:1 hs6 using "rauch.dta"
 drop if _merge==2
 drop _merge
 
**** lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop  if con=="n", /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if con=="r"|con=="w" , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
**** lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop  if con=="n", /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
reghdfe lnquantity treat2_post2 rta pergdp totpop if con=="r"|con=="w" , /// 
 absorb(year country_code) cluster(country_code)
 est store a4
**** lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop  if con=="n", /// 
 absorb(year country_code) cluster(country_code) 
 est store a5
reghdfe lnprice treat2_post2 rta pergdp totpop if con=="r"|con=="w" , /// 
 absorb(year country_code) cluster(country_code)
 est store a6
 
esttab  a1 a2 a3 a4 a5 a6 /* 
        */using "Hetero7_Belt132_Rauch_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N)  
 
 

/*================================================
        8.Lall（2000）-原材料、制成品
================================================*/

cd C:\papers\Belt
set more off

use "hg07_18_BR_reg_basic",clear
 gen hs6=substr(hs8,1,6)
*(6)剔除初级品和资源品 Lall（2000）
* A 非燃料初级产品 B 资源密集型制成品 C 低技能和技术密集型制成品 
* D 中等技能和技术密集型制成品 E 高技能和技术密集型制成品  F  矿物燃料 G 无类别产品
 merge m:1 hs6 using "prod_classification"
 drop productdescription codedescription
 drop if _merge==2
 drop _merge
 gen prod=1 if code=="A" | code=="F"
  replace prod=2 if code=="B"
  replace prod=3 if code=="C" | code=="D" | code=="E"
 tab prod
/*

       prod |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |    534,088       11.36       11.36
          2 |  1,307,556       27.81       39.17
          3 |  2,860,099       60.83      100.00
------------+-----------------------------------
      Total |  4,701,743      100.00
*/
 
 **** lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop if prod==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if prod==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
reghdfe lnexport treat2_post2 rta pergdp totpop if prod==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
**** lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop if prod==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a4
reghdfe lnquantity treat2_post2 rta pergdp totpop if prod==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a5
reghdfe lnquantity treat2_post2 rta pergdp totpop if prod==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a6
**** lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop if prod==1, /// 
 absorb(year country_code) cluster(country_code) 
 est store a7
reghdfe lnprice treat2_post2 rta pergdp totpop if prod==2 , /// 
 absorb(year country_code) cluster(country_code)
 est store a8
reghdfe lnprice treat2_post2 rta pergdp totpop if prod==3, /// 
 absorb(year country_code) cluster(country_code) 
 est store a9
esttab  a1 a2 a3 a4 a5 a6 a7 a8 a9 /* 
        */using "Hetero8_Belt132_prod_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 




/*================================================
        9.产能过剩行业
================================================*/
use "hg07_18_BR_reg_basic",clear
 gen hs6=substr(hs8,1,6)
 gen hs4=substr(hs8,1,4)
 gen hs2=substr(hs8,1,2)
 destring hs8 hs6 hs4 hs2,replace force
 
 gen coal=1 if hs4==2701|hs4==2702|hs4==2703|hs4==2704|hs4==2705|hs4==2706|hs4==2707|hs4==2708
 su coal // 3264
 gen cement=1 if hs4==2523
 su cement // 3,516
 gen steel=1 if (hs2==72&hs4!=7201)|hs2==73
 su steel // 352,786
 gen glass=1 if hs4==7003|hs4==7004|hs4==7005
 su glass // 12,052
 gen silicon=1 if hs8==28046190
 su silicon // 274
 gen windpower=1 if hs8==85030030|hs8==84129090|hs6==850231
 su windpower // 1,556 
 
 gen over_cap=1 if coal==1|cement==1|steel==1|glass==1|silicon==1|windpower==1
 replace over_cap=0 if over_cap==.
 tab over_cap
/*
   over_cap |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |  5,411,983       93.55       93.55
          1 |    373,448        6.45      100.00
------------+-----------------------------------
      Total |  5,785,431      100.00
*/
 
**** lnexport
reghdfe lnexport treat2_post2 rta pergdp totpop if over_cap==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a1
reghdfe lnexport treat2_post2 rta pergdp totpop if over_cap==1 , /// 
 absorb(year country_code) cluster(country_code)
 est store a2
**** lnquantity
reghdfe lnquantity treat2_post2 rta pergdp totpop if over_cap==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a3
reghdfe lnquantity treat2_post2 rta pergdp totpop if over_cap==1 , /// 
 absorb(year country_code) cluster(country_code)
 est store a4
**** lnprice
reghdfe lnprice treat2_post2 rta pergdp totpop if over_cap==0, /// 
 absorb(year country_code) cluster(country_code) 
 est store a5
reghdfe lnprice treat2_post2 rta pergdp totpop if over_cap==1 , /// 
 absorb(year country_code) cluster(country_code)
 est store a6
 
esttab  a1 a3 a5 a2 a4 a6 /* 
        */using "Hetero9_Belt132_overcap_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N) 

capture program drop rd_boot
program rd_boot, eclass
	tempvar touse
	gen byte `touse' = 1

    qui reghdfe lnexport treat2_post2 rta pergdp totpop if over_cap==0, /// 
                absorb(year country_code) cluster(country_code)    
	scalar rd_b = _b[treat2_post2]		
    qui reghdfe lnexport treat2_post2 rta pergdp totpop if over_cap==1, /// 
                absorb(year country_code) cluster(country_code)    
	scalar rd_a = _b[treat2_post2]
	scalar diff = rd_a - rd_b
		
	matrix b = (diff)
	matrix colnames b = diff
	ereturn post b , esample(`touse')
	ereturn display
end

rd_boot
bootstrap _b[diff], reps(30) seed(7): rd_boot 




/*================================================
               10.   产业上下游
================================================*/  

use "hg07_18_DD_upstream.dta",clear
 keep hs_hscd upstream iocd_2017
 duplicates drop
 rename hs_hscd hs8
 bysort hs8:egen a=mean( upstream )
 rename a upstream07
 duplicates drop hs8,force
 keep  hs8 upstream07 iocd_2017
 save "upstream07.dta",replace

use "hg07_18_DD_upstream02.dta",clear
 keep hs_hscd upstream iocd_2017
 duplicates drop
 rename hs_hscd hs8
 bysort hs8:egen a=mean( upstream )
 rename a upstream02
 duplicates drop hs8,force
 keep  hs8 upstream02 iocd_2017
 save "upstream02.dta",replace

use "hg07_18_BR_reg_basic",clear
 merge m:1 hs8 using "upstream07.dta"
 drop if _merge==2
 drop _merge
 rename iocd_2017 iocd_07
 merge m:1 hs8 using "upstream02.dta"
 drop if _merge==2
 drop _merge
 rename iocd_2017 iocd_02
 drop B_year B_month B_group belt65 tdummy1- datayear ///
  dist comlangoff iso3j gdpj rgdppcj countrynm _est_x1- _est_a1 /// 
  fproduct_num- lnprod_num
 save "hg07_18_BR_upstream",replace

use "hg07_18_BR_upstream",clear
 rename iocd_07 IO_code
 drop if IO_code==""
 bysort year country_code IO_code IO_code: egen V=total( export )
 bysort year country_code IO_code: egen Q=total( quantity )
 duplicates drop year country_code IO_code,force
 bysort year country_code: egen VV=total( V )
 bysort year country_code: egen QQ=total( Q )
 gen weightv=V/VV
 gen weightq=Q/QQ
 gen upstream_v=weightv*upstream07
 gen upstream_q=weightq*upstream07
 bysort year country_code: egen upstreamV_cty=total( upstream_v )
 bysort year country_code: egen upstreamQ_cty=total( upstream_q )
 duplicates drop year country_code,force
 save "hg07_18_BR_reg_upstream07",replace

 
use "hg07_18_BR_upstream",clear
 rename iocd_02 IO_code
 drop if IO_code==""
 bysort year country_code IO_code IO_code: egen V=total( export )
 bysort year country_code IO_code: egen Q=total( quantity )
 duplicates drop year country_code IO_code,force
 bysort year country_code: egen VV=total( V )
 bysort year country_code: egen QQ=total( Q )
 gen weightv=V/VV
 gen weightq=Q/QQ
 gen upstream_v=weightv*upstream07
 gen upstream_q=weightq*upstream07
 bysort year country_code: egen upstreamV_cty=total( upstream_v )
 bysort year country_code: egen upstreamQ_cty=total( upstream_q )
 duplicates drop year country_code,force
 save "hg07_18_BR_reg_upstream02",replace
**** lnexport

use "hg07_18_BR_reg_upstream02",clear
reghdfe upstreamV_cty treat2_post2 rta pergdp totpop , /// 
 absorb(year country_code) cluster(country_code) 
 est store a1

use "hg07_18_BR_reg_upstream07",clear
reghdfe upstreamV_cty treat2_post2 rta pergdp totpop , /// 
 absorb(year country_code) cluster(country_code) 
 est store a2
 
esttab  a1 a2 /* 
        */using "Hetero10_Belt132_upstream_DID.rtf", replace ar2(%9.4f) /* 
        */b(%9.4f) se(%9.4f) nogap  /* 
        */drop(rta pergdp totpop _cons) /* 
        */order(treat2_post2) /* 
        */ star(* 0.10  **  0.05  *** 0.01) /* 
        */title("Belt") scalar(N)







/*================================================
                  描述统计分析
================================================*/  

use "hg07_18_BR_reg1",clear
 drop treat1_d* treat2_d* tdummy* rtdummy*
 bysort year treat1:egen export65=total(export)
 bysort year treat1:egen quantity65=total(quantity)
 bysort year treat1:egen pricem65=mean(price)
 gen price65=export65/quantity65

 bysort year treat2:egen export132=total(export)
 bysort year treat2:egen quantity132=total(quantity)
 bysort year treat2:egen pricem132=mean(price)
 gen price132=export132/quantity132

 for any export65 quantity65 price65 export132 quantity132 ///
  price132 pricem65 pricem132:gen lnX=log(X)
 keep year treat1 treat2 lnexport65 lnquantity65 lnprice65 /// 
  lnexport132 lnquantity132 lnprice132 ///
  export65 quantity65 price65 export132 quantity132 price132 ///
  pricem65 pricem132 lnpricem65 lnpricem132
 duplicates drop year treat1 treat2,force
 save "statistic",replace
 
use "statistic",clear
 duplicates drop year treat2,force
 sort treat2 year 
 bysort year:egen texp=total(export132)
 bysort year:egen tquan=total(quantity132)
 gen share_exp132=export132/texp
 gen share_quan132=quantity132/tquan

 for any texp export132:replace X=X/10000000000
 
 
 label var share_exp132 "export share"
twoway (line share_exp132 year if treat2==0,lpattern(solid) col(gray)) ///
 (line share_exp132 year if treat2==1,lpattern(dash) col(black)), ///
 xlabel(2007(3)2016 2018) /// 
 graphregion(color(white)) legend(label(1 "其他国家") label(2 "一带一路沿线国家"))
 graph save "trend_lnexport132",replace
 
 label var share_quan132 "quantity share"
twoway (line share_quan132 year if treat2==0,lpattern(solid) col(gray)) ///
 (line share_quan132 year if treat2==1,lpattern(dash) col(black)), ///
 xlabel(2007(3)2016 2018) /// 
 graphregion(color(white)) legend(label(1 "其他国家") label(2 "一带一路沿线国家"))
 graph save "trend_lnquantity132",replace

 label var lnpricem132 "lnprice"
twoway (line lnpricem132 year if treat2==0,lpattern(solid) col(gray)) ///
 (line lnpricem132 year if treat2==1,lpattern(dash) col(black)), ///
 xlabel(2007(3)2016 2018) ylabel(, format(%2.1f)) /// 
 graphregion(color(white)) legend(label(1 "其他国家") label(2 "一带一路沿线国家"))
 graph save "trend_lnprice132",replace
 

 
use "statistic",clear
 duplicates drop year treat1,force
 sort treat1 year  
 bysort year:egen texp=total(export65)
 bysort year:egen tquan=total(quantity65)
 gen share_exp65=export65/texp
 gen share_quan65=quantity65/tquan
  
 label var share_exp65 "export share"
twoway (line share_exp65 year if treat1==0,lpattern(solid) col(gray)) ///
 (line share_exp65 year if treat1==1,lpattern(dash) col(black)), ///
 xlabel(2007(3)2016 2018) ylabel(, format(%2.1f)) /// 
 graphregion(color(white)) legend(label(1 "其他国家") label(2 "一带一路沿线国家"))
 graph save "trend_lnexport65",replace
 
 label var share_quan65 "quantity share"
twoway (line share_quan65 year if treat1==0,lpattern(solid) col(gray)) ///
 (line share_quan65 year if treat1==1,lpattern(dash) col(black)), ///
 xlabel(2007(3)2016 2018) /// 
 graphregion(color(white)) legend(label(1 "其他国家") label(2 "一带一路沿线国家"))
 graph save "trend_lnquantity65",replace

 label var lnpricem65 "lnprice"
twoway (line lnpricem65 year if treat1==0,lpattern(solid) col(gray)) ///
 (line lnpricem65 year if treat1==1,lpattern(dash) col(black)), ///
 xlabel(2007(3)2016 2018) ylabel(, format(%2.1f)) /// 
 graphregion(color(white)) legend(label(1 "其他国家") label(2 "一带一路沿线国家"))
 graph save "trend_lnprice65",replace
