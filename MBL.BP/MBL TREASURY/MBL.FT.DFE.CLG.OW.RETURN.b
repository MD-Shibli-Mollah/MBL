* @ValidationCode : MjotMTQ4ODI2NjI4NDpDcDEyNTI6MTYzMjA1OTgwNzYxODp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 19 Sep 2021 19:56:47
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE MBL.FT.DFE.CLG.OW.RETURN

*-----------------------------------------------------------------------------
* Description     : This is a Before Auth Routine
* Attached Version: FUNDS.TRANSFER,MBL.DFE.CHQ.CLG.OW.R
*-----------------------------------------------------------------------------
* Revision History:
*------------------
* Date         - 20/05/2021
* Done By      - MD SHIBLI MOLLAH

*** <region name= Inserts>
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DFE.FRAME.WORK.COMMON
    $INSERT I_F.DFE.OUTWARD.WORK.FILE
    $INSERT I_DFE.OUTWARD.FILE.EXTRACT.COMMON
    
    $USING FT.Contract
    $USING EB.SystemTables
    $USING EB.Utility
    $USING EB.Interface
    $USING EB.DataAccess
    $USING EB.Foundation
    
*** </region>
*-----------------------------------------------------------------------------
*** <region name= Main Control Logic>
    GOSUB INITIALISE
    GOSUB PROCESS

RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= Initialise>
*** <desc>Common variables are initialised</desc>
INITIALISE:
    TODAY = EB.SystemTables.getToday()

* Y.CURRENT.RECORD = C$CURRENT.RECORD
*    Y.TR.CODE = FIELD(C$CURRENT.RECORD,",",1)
*    Y.CURRENCY = FIELD(C$CURRENT.RECORD,",",3)
*    Y.COM = FIELD(C$CURRENT.RECORD,",",19)
*    Y.CR.ACC.NUM = FIELD(C$CURRENT.RECORD,",",2)
*    Y.DR.DT = FIELD(C$CURRENT.RECORD,",",5)
*    Y.DEBIT.ACCT.ID = FIELD(C$CURRENT.RECORD,",",6)
*    Y.D.AMT = FIELD(C$CURRENT.RECORD,",",4)
*    Y.PAY.DET = FIELD(C$CURRENT.RECORD,",",7)
*    Y.CRG.ACC.NO = FIELD(C$CURRENT.RECORD,",",10)
*    Y.COM.CODE = FIELD(C$CURRENT.RECORD,",",11)
*    Y.COM.TP = FIELD(C$CURRENT.RECORD,",",12)
*    Y.LT.CHQ.NO = FIELD(C$CURRENT.RECORD,",",14)
*    Y.LT.CHQ.DT = FIELD(C$CURRENT.RECORD,",",13)
*    Y.LT.ROUTE.CODE = FIELD(C$CURRENT.RECORD,",",15)
*    Y.LT.CLG.TYPE = FIELD(C$CURRENT.RECORD,",",16)
*    Y.LT.CPS.REF.NO = FIELD(C$CURRENT.RECORD,",",17)
*    Y.LT.BB.CHQ.TYPE = FIELD(C$CURRENT.RECORD,",",18)

    Y.APP.NAME ="FUNDS.TRANSFER"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.FT.DR.NARR":@VM:"LT.FT.CR.NARR"
    FLD.POS = ""
    EB.Foundation.MapLocalFields(Y.APP.NAME,LOCAL.FIELDS,FLD.POS)
    Y.LT.FT.DR.NARR.POS=FLD.POS<1,1>
    Y.LT.FT.CR.NARR.POS=FLD.POS<1,2>
    FLD.POS = ""
    
*---------------------------------------------------------------------------
    Y.TR.CODE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TransactionType)
    Y.CURRENCY = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCurrency)
    Y.COM = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CoCode)
* BDT140250 0010101 - BNK -BD0010101 -MAIN - ISL - BD0013101
    
    Y.DR.COM = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCompCode)
    Y.CR.COM = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditCompCode)
    Y.CR.ACC.NUM = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
    Y.DR.DT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
    Y.DEBIT.ACCT.ID = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
    Y.D.AMT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)
    Y.PAY.DET = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.PaymentDetails)
    Y.TEMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
    Y.LT.FT.DR.NARR = Y.TEMP<1,Y.LT.FT.DR.NARR.POS>
    Y.LT.FT.CR.NARR = Y.TEMP<1,Y.LT.FT.CR.NARR.POS>
*------------------------------------------------------------------------

RETURN

*** </region>
*----------------------------------------------------------------------------------
*** <region name= PROCESS>
***<desc>Based on mapping id, Update batch or entries details</desc>
*-------
PROCESS:
*
*----------------DEBIT FROM CUS ACC USING OFS --------------------------------------------------------
    OFS.SOURCE = 'MBL.PRE.OFS'
                
    Y.MESSAGE = "FUNDS.TRANSFER,MBL.DFE.CHQ.CLG.OW.H/I/PROCESS//0,//":Y.DR.COM:"///////////////,,TRANSACTION.TYPE:1:1=":Y.TR.CODE:",DEBIT.ACCT.NO:1:1=":Y.DEBIT.ACCT.ID:",DEBIT.CURRENCY:1:1=":Y.CURRENCY:",DEBIT.AMOUNT:1:1=":Y.D.AMT:",DEBIT.VALUE.DATE:1:1=":Y.DR.DT:",CREDIT.ACCT.NO:1:1=":Y.CR.ACC.NUM:",PAYMENT.DETAILS:1:1=":Y.PAY.DET:",LT.FT.DR.NARR:1:1=":Y.LT.FT.DR.NARR:",LT.FT.CR.NARR:1:1=":Y.LT.FT.CR.NARR
    EB.Interface.OfsCallBulkManager(OFS.SOURCE, Y.MESSAGE, OFS.RES, TXN.VAL)
 
*----------------------------------TRACER---------------------------------------------------------------
*    WriteData = 'Y.COM: ':Y.COM:' Y.DR.COM: ':Y.DR.COM:'Y.CR.COM: ':Y.CR.COM
*    FileName = 'DFE.RETURN.SEP.21.txt'
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

*----------------------------------END----------------------------------------------------------------
RETURN
END