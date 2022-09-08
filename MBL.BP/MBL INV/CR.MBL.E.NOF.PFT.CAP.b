* @ValidationCode : Mjo5NjI4NzI0ODg6Q3AxMjUyOjE2MDIwOTA1NzQ2NTc6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 07 Oct 2020 23:09:34
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CR.MBL.E.NOF.PFT.CAP(Y.RETURN)
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Developed by MD. KAMRUL HASAN -- FDS -- on 07TH OCT
*-----------------------------------------------------------------------------
    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $USING EB.DataAccess
    $USING AA.Framework
    $USING AA.Interest
    $USING  EB.Reports
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
    
    
**-----------------------------------------------------------------------------
INIT:
*-----------------------------------------------------------------------------
    LOCATE "ARR.ID" IN EB.Reports.getEnqSelection()<2,1> SETTING ARR.POS THEN
        Y.ARR.ID=EB.Reports.getEnqSelection()<4,ARR.POS>
    END
    
    FN.AA.INT.ACC='F.AA.INTEREST.ACCRUALS'
    F.AA.INT.ACC=''
    
    FN.AA.ARRANGEMENT='F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT=''
    
RETURN
**-----------------------------------------------------------------------------
OPENFILES:
*-----------------------------------------------------------------------------
    
    EB.DataAccess.Opf(FN.AA.INT.ACC,F.AA.INT.ACC)
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)




RETURN
**-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
    
    Y.MNEMONIC = FN.AA.INT.ACC[2,3]
    IF Y.MNEMONIC EQ 'BNK' THEN
        Y.CUR.ACC = 'PRINCIPALINT'
        Y.PRP.OD = 'INTONOD'
    END
    IF Y.MNEMONIC EQ 'ISL' THEN
        Y.CUR.ACC = 'DEFERREDPFT'
        Y.PRP.OD='PFTONOD'
    END

*========================PRINCIPALINT=========================================
    Y.PRNC.ID = Y.ARR.ID:'-':Y.CUR.ACC
    EB.DataAccess.FRead(FN.AA.INT.ACC,Y.PRNC.ID,REC.AA.INT.ACC,F.AA.INT.ACC,ERR.AA.INT.ACC)
    Y.ACR.AMT=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccAccrualAmt>
    Y.ACR.FROM.DATE=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccFromDate>
    Y.ACR.TO.DATE=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccToDate>
    Y.ACR.RATE=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccRate>
    Y.ACR.DAYS=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccDays>
    Y.ACR.BALANCE=REC.AA.INT.ACC<AA.Interest.InterestAccruals.IntAccBalance>
    
    Y.PRNC.DCOUNT = DCOUNT(Y.ACR.FROM.DATE,VM)
    
    FOR X=1 TO Y.PRNC.DCOUNT
    
        Y.FROM.DATE=Y.ACR.FROM.DATE<1,X>
        Y.TO.DATE=Y.ACR.TO.DATE<1,X>
        Y.ACCR.AMT = Y.ACR.AMT<1,X>
        Y.RATE.T = Y.ACR.RATE<1,X>
        Y.BALANCE = Y.ACR.BALANCE<1,X>
        Y.DAYS= Y.ACR.DAYS<1,X>
        GOSUB ARR.PRNC.PRINT
    NEXT X
*=================================================================================
*-=========================ODPROFIT==============================================
    
    Y.OD.ID = Y.ARR.ID:'-':Y.PRP.OD
    EB.DataAccess.FRead(FN.AA.INT.ACC,Y.OD.ID,REC.AA.INT.ACC.OD,F.AA.INT.ACC,ERR.AA.INT.ACC.OD)
    Y.ACR.AMT=REC.AA.INT.ACC.OD<AA.Interest.InterestAccruals.IntAccTotAccrAmt>
    Y.ACR.FROM.DATE=REC.AA.INT.ACC.OD<AA.Interest.InterestAccruals.IntAccPeriodStart>
    Y.ACR.TO.DATE=REC.AA.INT.ACC.OD<AA.Interest.InterestAccruals.IntAccToDate>
    Y.ACR.RATE=REC.AA.INT.ACC.OD<AA.Interest.InterestAccruals.IntAccRate>
    Y.ACR.DAYS=REC.AA.INT.ACC.OD<AA.Interest.InterestAccruals.IntAccDays>
    Y.ACR.BALANCE=REC.AA.INT.ACC.OD<AA.Interest.InterestAccruals.IntAccTotPosAccrAmt>
    
    Y.OD.DCOUNT = DCOUNT(Y.ACR.FROM.DATE,VM)
    
*    PROP.CLASS = 'INTEREST'
*    PROPERTY= Y.PRP.OD
*    AA.Framework.GetArrangementConditions(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
*    R.INT.SPRD = RAISE(RETURN.VALUES)
*
*    Y.INT.SPRD = R.INT.SPRD<AA.Interest.Interest.IntEffectiveRate>
    
    FOR Y=1 TO Y.OD.DCOUNT
    
        Y.FROM.DATE=Y.ACR.FROM.DATE<1,Y>
        Y.TO.DATE=Y.ACR.TO.DATE<1,Y>
        Y.ACCR.AMT = Y.ACR.AMT<1,Y>
        Y.RATE.T = Y.ACR.RATE<1,Y>
        Y.BALANCE = Y.ACR.BALANCE<1,Y>
        Y.DAYS= Y.ACR.DAYS<1,Y>
        GOSUB ARR.OD.PRINT
    NEXT Y
    
    IF Y.PRNC.DCOUNT GT Y.OD.DCOUNT THEN
        Y.MAX.DCOUNT = Y.PRNC.DCOUNT
    END
    ELSE
        Y.MAX.DCOUNT = Y.OD.DCOUNT
    END
    
    FOR Z=1 TO Y.MAX.DCOUNT
        Y.RETURN<-1> = Y.PRNC.RETURN<Z>:'*':Y.OD.RETURN<Z>
            
    NEXT Z
    
    
RETURN
*---------
ARR.PRNC.PRINT:
*----------

    Y.PRNC.RETURN<-1>= Y.FROM.DATE:'*':Y.TO.DATE:'*':Y.DAYS:'*':Y.BALANCE:'*':Y.RATE.T:'*':Y.ACCR.AMT

    Y.FROM.DATE=''
    Y.TO.DATE=''
    Y.RATE=''
    Y.DAYS=''
    Y.BALANCE=''
    Y.ACCR.AMT=''
RETURN
*---------
ARR.OD.PRINT:
*----------

    Y.OD.RETURN<-1>= Y.FROM.DATE:'*':Y.TO.DATE:'*':Y.DAYS:'*':Y.BALANCE:'*':Y.RATE.T:'*':Y.ACCR.AMT

    Y.FROM.DATE=''
    Y.TO.DATE=''
    Y.RATE=''
    Y.DAYS=''
    Y.BALANCE=''
    Y.ACCR.AMT=''
RETURN
END
