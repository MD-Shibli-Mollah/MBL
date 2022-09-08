SUBROUTINE TF.MBL.A.PC.COM.AMT.AMD
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

    EB.LocalReferences.GetLocRef('AA.PRD.DES.ACCOUNT','LT.TF.JOB.NUMBR',Y.JOB.NUM.POS)
    EB.LocalReferences.GetLocRef('AA.PRD.DES.ACCOUNT','LT.TF.EXCH.RATE',Y.EXCH.RATE.POS)
RETURN

PROCESS:
   
*GET ARR ACCOUNT PROPERTY FILED DATA..........................................
    APP.NAME = 'AA.ARR.ACCOUNT'
    EB.API.GetStandardSelectionDets(APP.NAME, R.SS)
    Y.FIELD.NAME = 'LOCAL.REF'
    LOCATE Y.FIELD.NAME IN R.SS<AA.Account.Account.AcLocalRef> SETTING Y.POS THEN
    END
    CALL AA.GET.ACCOUNT.RECORD(R.PROPERTY.RECORD, PROPERTY.ID)
    TMP.DATA = R.PROPERTY.RECORD<1,Y.POS>
    Y.JOB.NUM =   FIELD(TMP.DATA,SM, Y.JOB.NUM.POS)
    Y.EXCH.RATE = FIELD(TMP.DATA,SM, Y.EXCH.RATE.POS)

     

    
*GET ARR COMITMMENT PROPERTY FILED DATA..........................................
    
    Y.ARR.ID = AA.Framework.getC_aalocarrid()
    Y.CNG.AMT = EB.SystemTables.getRNew(AA.TermAmount.TermAmount.AmtChangeAmount)
    Y.DOC.FC.AMT = Y.CNG.AMT / Y.EXCH.RATE

    Y.JOB.NUMBER = Y.JOB.NUM
    EB.DataAccess.FRead(FN.BD.BTB.JOB.REGISTER,Y.JOB.NUMBER,R.BTB.JOB.REGISTER,F.BD.BTB.JOB.REGISTER,Y.BD.JOB.REG.ERR)
    IF R.BTB.JOB.REGISTER THEN
        Y.PC.LD.REF = R.BTB.JOB.REGISTER<BTB.JOB.PCECC.LOAN.ID>
        LOCATE Y.ARR.ID IN Y.PC.LD.REF<1,1> SETTING Y.CNT.POS THEN
            Y.COUNT = Y.CNT.POS
            R.BTB.JOB.REGISTER<BTB.JOB.LOAN.AMT.FCY,Y.COUNT> += DROUND(Y.DOC.FC.AMT,2)
            R.BTB.JOB.REGISTER<BTB.JOB.LOAN.AMT.LCY,Y.COUNT> += DROUND(Y.CNG.AMT,2)
            R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.AMT> += DROUND(Y.DOC.FC.AMT,2)
            R.BTB.JOB.REGISTER<BTB.JOB.TOT.PC.AVL.AMT> -= DROUND(Y.DOC.FC.AMT,2)

            WRITE R.BTB.JOB.REGISTER TO F.BD.BTB.JOB.REGISTER, Y.JOB.NUMBER
        END
    END
**********************************************

RETURN
  


END
