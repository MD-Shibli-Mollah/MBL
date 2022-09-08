* @ValidationCode : MjotMTM3NDM0MTE1MjpDcDEyNTI6MTYyODUyMzM5MzY1OTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 09 Aug 2021 21:36:33
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE CR.MBL.V.AA.EXP.LIM

*-----------------------------------------------------------------------------
* Modification History :
*Subroutine Description: Validation routine for Expired Loan(AA ID or Acc Num)
*Subroutine Type:
*Attached To    :  ACTIVITY.API - MBL.ACCT.TITLE.API, Property Class: LIMIT, Action: UPDATE
*Attached As    : RECORD ROUTINE
*-----------------------------------------------------------------------------
* Modification History 1 :
* 04/08/2021 -                            Developed By   - MD. Shibli Mollah
*                                                          FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
    
    $USING AC.AccountOpening
    $USING AA.Limit
    $USING EB.SystemTables
    $USING AA.Framework
    $USING EB.Updates
    $USING EB.DataAccess
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''

    FLD.POS = ''
RETURN
*-----------------------------------------------------------------------------

OPENFILES:

    EB.DataAccess.Opf(FN.ACC, F.ACC)

RETURN
*-----------------------------------------------------------------------------

PROCESS:
    
*--------Map Local fields----------------------------------------------------------------
    APPLICATION.NAME = 'AA.ARRANGEMENT.ACTIVITY'
    LOCAL.FIELDS = 'LT.EXP.LN.ID'
    EB.Updates.MultiGetLocRef(APPLICATION.NAME, LOCAL.FIELDS, FLD.POS)
    Y.EXP.LN.ID.POS = FLD.POS<1,1>
*--------END----------------------------------------------------------------

    Y.LOC.DATA = c_aalocArrActivityRec<AA.Framework.ArrangementActivity.ArrActLocalRef>
    Y.AA.ID = Y.LOC.DATA<1, Y.EXP.LN.ID.POS>
    
    Y.AA.LEN = Y.AA.ID[1,2]
*Y.AA.ID = c_aalocArrId

*-----------------------AA ID CHECK-----------------------------------------
    IF Y.AA.LEN NE 'AA' THEN
        Y.ACC = Y.AA.ID
    
        EB.DataAccess.FRead(FN.ACC, Y.ACC, R.ACC, F.ACC, ACC.ERR)
* Y.CUS.ID = R.ACC<AC.AccountOpening.Account.Customer>
        Y.AA.ID = R.ACC<AC.AccountOpening.Account.ArrangementId>
    END
*---------------------------END-------------------------------------------

*-------------------------------AA.ARR.LIMIT-----------------------------------------------
    PROP.CLASS.LIM = 'LIMIT'
    AA.Framework.GetArrangementConditions(Y.AA.ID, PROP.CLASS.LIM, PROPERTY, '', RETURN.IDS.LIM, RETURN.VALUES.LIM, ERR.MSG.LIM)
    R.LIMIT = RAISE(RETURN.VALUES.LIM)
    Y.LIM.REF = R.LIMIT<AA.Limit.Limit.LimLimitReference>
    Y.LIM.SER = R.LIMIT<AA.Limit.Limit.LimLimitSerial>

**SET LIMIT DATA
    EB.SystemTables.setRNew(AA.Limit.Limit.LimLimitReference, Y.LIM.REF)
    EB.SystemTables.setRNew(AA.Limit.Limit.LimLimitSerial, Y.LIM.SER)
    
*-------------------------------TRACER-------------------------------------------------
*    WriteData = Y.AA.ID:" R.LIMIT: ":R.LIMIT:" Y.LIM.REF: ":Y.LIM.REF:" Y.LIM.SER: ":Y.LIM.SER
*    FileName = 'SHIBLI_CR.AA.LIMIT.txt'
*    FilePath = 'DL.BP'
*    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
*    ELSE
*        CREATE FileOutput ELSE
*        END
*    END
*    WRITESEQ WriteData APPEND TO FileOutput ELSE
*        CLOSESEQ FileOutput
*    END
*    CLOSESEQ FileOutput

*-------------------------------TRACER END-------------------------------------------------

RETURN

END