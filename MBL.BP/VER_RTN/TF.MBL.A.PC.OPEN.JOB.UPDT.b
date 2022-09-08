SUBROUTINE TF.MBL.A.PC.OPEN.JOB.UPDT
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History :
* 07/11/2020 -                            Retrofit   - MAHMUDUR RAHMAN UDOY,
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
*
    $INCLUDE I_EQUATE
    $INCLUDE I_COMMON
    $INCLUDE I_F.BD.BTB.JOB.REGISTER
    
    $USING LC.Contract
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING AA.Framework
    $USING AA.Account
    $USING AA.TermAmount
    $USING EB.LocalReferences
    $USING EB.API
  
    Y.FUC = EB.SystemTables.getVFunction()

    
    IF Y.FUC EQ 'A' THEN

        GOSUB INITIALISE
        GOSUB PROCESS
    END

INITIALISE:

    FN.BD.BTB.JOB.REGISTER = 'F.BD.BTB.JOB.REGISTER'
    F.BD.BTB.JOB.REGISTER = ''
    EB.DataAccess.Opf(FN.BD.BTB.JOB.REGISTER,F.BD.BTB.JOB.REGISTER)

RETURN

PROCESS:
    GOSUB GET.LOC.REF.POS
    
    
*    IF EB.SystemTables.getRNew(AA.Account.Account.AcLocalRef)<1,Y.JOB.NUM.POS> EQ "" THEN RETURN
    
*GET ARR ACCOUNT PROPERTY FILED DATA..........................................
    APP.NAME = 'AA.ARR.ACCOUNT'
    EB.API.GetStandardSelectionDets(APP.NAME, R.SS)
    Y.FIELD.NAME = 'LOCAL.REF'
    LOCATE Y.FIELD.NAME IN R.SS<AA.Account.Account.AcLocalRef> SETTING Y.POS THEN
    END
    CALL AA.GET.ACCOUNT.RECORD(R.PROPERTY.RECORD, PROPERTY.ID)
    TMP.DATA = R.PROPERTY.RECORD<1,Y.POS>
    Y.JOB.NUM =   FIELD(TMP.DATA,SM, Y.JOB.NUM.POS)
    Y.JOB.ENTAM = FIELD(TMP.DATA,SM, Y.JOB.ENTAM.POS)
    Y.EXCH.RATE = FIELD(TMP.DATA,SM, Y.EXCH.RATE.POS)
    Y.JOB.ENCUR = FIELD(TMP.DATA,SM, Y.JOB.ENCUR.POS)
    Y.DOC.VL =    FIELD(TMP.DATA,SM, Y.DOC.VL.POS)
     

    
*GET ARR COMITMMENT PROPERTY FILED DATA..........................................
    
    Y.ARR.ID = AA.Framework.getC_aalocarrid()
    
    PROP.CLASS = 'TERM.AMOUNT'
    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ARR.ID,PROP.CLASS,PROPERTY,'',RETURN.IDS,RETURN.VALUES,ERR.MSG)
    AC.R.REC = RAISE(RETURN.VALUES)
    Y.AMT = AC.R.REC<AA.TermAmount.TermAmount.AmtAmount>
    Y.MAT.DATE = AC.R.REC<AA.TermAmount.TermAmount.AmtMaturityDate>
    


    Y.JOB.NUMBER = Y.JOB.NUM
    Y.PC.LD.AMT = Y.DOC.VL
    EB.DataAccess.FRead(FN.BD.BTB.JOB.REGISTER,Y.JOB.NUMBER,R.BTB.JOB.REGISTER,F.BD.BTB.JOB.REGISTER,Y.BD.JOB.REG.ERR)
    IF NOT(Y.BD.JOB.REG.ERR) THEN
        Y.PC.LD.REF = R.BTB.JOB.REGISTER<BTB.JOB.PCECC.LOAN.ID>
        IF NOT(Y.PC.LD.REF) THEN
            Y.COUNT = 1
            Y.TOT.PC.LD.AMT = "0"
        END ELSE
            LOCATE Y.ARR.ID IN Y.PC.LD.REF<1,1> SETTING Y.CNT.POS THEN
                Y.COUNT = Y.CNT.POS
                Y.TOT.PC.LD.AMT = R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.AMT> - R.BTB.JOB.REGISTER<BTB.JOB.LOAN.AMT.FCY,Y.COUNT>
                R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.AVL.AMT> += R.BTB.JOB.REGISTER<BTB.JOB.LOAN.AMT.FCY,Y.COUNT>

            END ELSE
                Y.COUNT = DCOUNT(Y.PC.LD.REF,@VM)
                Y.COUNT += 1
                Y.TOT.PC.LD.AMT = R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.AMT>

            END
        END
        GOSUB UPDATE.JOB.REGISTER
    END
**********************************************

RETURN
   
UPDATE.JOB.REGISTER:
*SET PC PRODUCT ARR FILED DATA TO JOB.REGISTER APPLICATON.....................................
    

    R.BTB.JOB.REGISTER<BTB.JOB.PCECC.LOAN.ID,Y.COUNT> = Y.ARR.ID
    R.BTB.JOB.REGISTER<BTB.JOB.LOAN.FC.CUR,Y.COUNT> = Y.JOB.ENCUR
    R.BTB.JOB.REGISTER<BTB.JOB.LOAN.AMT.FCY,Y.COUNT> = DROUND(Y.DOC.VL,2)
    R.BTB.JOB.REGISTER<BTB.JOB.LOAN.EX.RT,Y.COUNT> = Y.EXCH.RATE
    R.BTB.JOB.REGISTER<BTB.JOB.LOAN.AMT.LCY,Y.COUNT> = DROUND(Y.AMT,2)
    R.BTB.JOB.REGISTER<BTB.JOB.LOAN.MAT.DT,Y.COUNT> = Y.MAT.DATE
    Y.JOB.TOT.PC.AMT = Y.PC.LD.AMT + Y.TOT.PC.LD.AMT
    R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.AMT> = DROUND(Y.JOB.TOT.PC.AMT,2)
    R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.AVL.AMT> -= DROUND(Y.PC.LD.AMT,2)
 
    WRITE R.BTB.JOB.REGISTER TO F.BD.BTB.JOB.REGISTER, Y.JOB.NUMBER
    
RETURN

GET.LOC.REF.POS:

    EB.LocalReferences.GetLocRef('AA.PRD.DES.ACCOUNT','LT.TF.JOB.NUMBR',Y.JOB.NUM.POS)
    EB.LocalReferences.GetLocRef('AA.PRD.DES.ACCOUNT','LT.TF.JOB.ENTAM',Y.JOB.ENTAM.POS)
    EB.LocalReferences.GetLocRef('AA.PRD.DES.ACCOUNT','LT.TF.EXCH.RATE',Y.EXCH.RATE.POS)
    EB.LocalReferences.GetLocRef('AA.PRD.DES.ACCOUNT','LT.TF.JOB.ENCUR',Y.JOB.ENCUR.POS)
    EB.LocalReferences.GetLocRef('AA.PRD.DES.ACCOUNT','LT.TF.DOC.VL.FC',Y.DOC.VL.POS)
 
RETURN


END