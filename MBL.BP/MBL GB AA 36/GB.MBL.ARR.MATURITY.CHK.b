SUBROUTINE GB.MBL.ARR.MATURITY.CHK
*-----------------------------------------------------------------------------
*Subroutine Description: THIS ROUTINE USED FOR DEPOSIT ARRANGEMENT MATURITY CHECK
*Subroutine Type:
*Attached To    :
*Attached As    :
*-----------------------------------------------------------------------------
* Created History :
* 25/08/2020 -                            Created   - MD. KAMRUL HASAN,
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    
    $USING AA.Framework
    $USING AA.TermAmount
    $USING EB.LocalReferences
    $USING EB.DataAccess
    $USING AA.PaymentSchedule
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
************************************
INIT:
************************************
    FN.APPL.NAME = 'F.AA.ARR.TERM.AMOUNT'
    F.APPL.NAME=""
    Y.LT.MARK = 'LT.DP.ARR.ID'
    Y.LT.MARK.POS =''

    FN.AA.ARR='F.AA.ARRANGEMENT'
    F.AA.ARR= ''
    
    FN.AA.ACC.DETAILS = 'F.AA.ACCOUNT.DETAILS'
    F.AA.ACC.DETAILS = ''
    
RETURN
************************************
OPENFILES:
************************************
   
    EB.DataAccess.Opf(FN.AA.ARR,F.AA.ARR)
    EB.DataAccess.Opf(FN.AA.ACC.DETAILS,F.AA.ACC.DETAILS)
    EB.DataAccess.Opf(FN.APPL.NAME, F.APPL.NAME)

RETURN
************************************
PROCESS:
************************************

    Y.RECORD.STATUS = AA.Framework.getC_aalocactivitystatus()
    IF Y.RECORD.STATUS EQ 'AUTH' THEN
        Y.ARR.ID = AA.Framework.getC_aalocarrid()

    
        EB.LocalReferences.GetLocRef("AA.ARR.TERM.AMOUNT",Y.LT.MARK,Y.LT.MARK.POS)

        Y.TOT.LOCAL.FIELD = EB.SystemTables.getRNew(AA.TermAmount.TermAmount.AmtLocalRef)
        Y.DEPOSIT.ARR.ID = Y.TOT.LOCAL.FIELD<1,Y.LT.MARK.POS>
        Y.LN.MAT.DATE = EB.SystemTables.getRNew(AA.TermAmount.TermAmount.AmtMaturityDate)
    
        IF Y.DEPOSIT.ARR.ID THEN
            EB.DataAccess.FRead(FN.AA.ACC.DETAILS,Y.DEPOSIT.ARR.ID,R.AA.ARR,F.AA.ACC.DETAILS,Y.ARR.ERR)
            IF R.AA.ARR EQ '' THEN
            
                SEL.ID.CMD = "SELECT ":FN.AA.ARR:" WITH LINKED.APPL.ID EQ ":Y.DEPOSIT.ARR.ID
                EB.DataAccess.Readlist(SEL.ID.CMD, SEL.LIST, ListName, NO.OF.REC, SystemReturnCode)
                Y.DEPOSIT.ARR.ID = SEL.LIST
           
                EB.DataAccess.FRead(FN.AA.ACC.DETAILS,Y.DEPOSIT.ARR.ID,R.AA.ARR,F.AA.ACC.DETAILS,Y.ARR.ERR)
            END
    
        END ELSE
            RETURN
        END
    
        Y.DP.MAT.DATE = R.AA.ARR<AA.PaymentSchedule.AccountDetails.AdMaturityDate>
    

        SEL.CMD = "SELECT ":FN.APPL.NAME:" WITH @ID LIKE ":Y.DEPOSIT.ARR.ID:"-":"COMMITMENT-..."
        
        EB.DataAccess.Readlist(SEL.CMD, SEL.LIST,"",NO.OF.RECORD,RET.CODE)
        Y.TERM.ID = SEL.LIST<1,NO.OF.RECORD>
        
        EB.DataAccess.FRead(FN.APPL.NAME, Y.TERM.ID, REC.ARR, F.APPL.NAME, ERR)
        Y.TOT.LOCAL.FIELD.DP = REC.ARR<AA.TermAmount.TermAmount.AmtLocalRef>
        Y.TOT.LOCAL.FIELD.DP<1,Y.LT.MARK.POS> = Y.ARR.ID
        REC.ARR<AA.TermAmount.TermAmount.AmtLocalRef> = Y.TOT.LOCAL.FIELD.DP
              
        WRITE REC.ARR TO F.APPL.NAME,Y.TERM.ID

        
    END
  
RETURN
END
