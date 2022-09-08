* @ValidationCode : MjotNDg3MTQ1MDc4OkNwMTI1MjoxNjEyNDYzMDQwMTgxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Feb 2021 00:24:00
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.A.PO.ISSU.CHQ.REG.SUP.WRT
**Subroutine Description:
* THIS ROUTINE USE FOR UPDATE PO ISSUE INFORMATION TO CHEQUE.REGISTER.SUPPLEMENT
* BASE ON GIVEN ISSUED PO ORDER NO
*Attached To    : VERSION(FUNDS.TRANSFER,MBL.PO.ISSUE TELLER,MBL.PO.SELL.CASH)
*Attached As    : AUTH ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 05/11/2020 -                                NEW - MD. SAROWAR MORTOZA
*                                             FDS Bangladesh Limited
***FINAL UPDATE: REQUIRED FOR TELLER(115)  BY
* 04/02/2021                                  MD.SHIBLI MOLLAH
*                                             FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING FT.Contract
    $USING TT.Contract
    $USING ST.ChqSubmit
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Updates
    $USING EB.TransactionControl
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *
    GOSUB OPENFILE ; *
    GOSUB PROCESS ; *
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc> </desc>
    FN.CHQ.REG.SUP='F.CHEQUE.REGISTER.SUPPLEMENT'
    F.CHQ.REG.SUP=''
    FN.TELLER='F.TELLER'
    F.TELLER=''
    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.TELLER.NAU='F.TELLER$NAU'
    F.TELLER.NAU=''
    FN.FT.NAU='F.FUNDS.TRANSFER$NAU'
    F.FT.NAU=''
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc> </desc>
    EB.DataAccess.Opf(FN.CHQ.REG.SUP, F.CHQ.REG.SUP)
    EB.DataAccess.Opf(FN.TELLER, F.TELLER)
    EB.DataAccess.Opf(FN.FT, F.FT)
    EB.DataAccess.Opf(FN.TELLER.NAU, F.TELLER.NAU)
    EB.DataAccess.Opf(FN.FT.NAU, F.FT.NAU)
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>
    GOSUB GET.LOCAL.REF ; *

    IF EB.SystemTables.getApplication() EQ 'TELLER' THEN
        Y.PO.PUR.NAME=EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.PUR.NAME.POS>
        Y.PO.AMT=EB.SystemTables.getRNew(TT.Contract.Teller.TeChrgAmtLocal)
        Y.PO.COMM.AMT=FIELD(Y.PO.AMT,VM,1)
        Y.PO.VAT.AMT=FIELD(Y.PO.AMT,VM,2)
        Y.PO.AC=EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
        Y.PO.NO=EB.SystemTables.getRNew(TT.Contract.Teller.TeStockNumber)
        
*-----------REV---------------VAR------------------------------------------
        Y.CRS.PO.PUR.NAME = Y.PO.PUR.NAME
        Y.CRS.PO.COMM.AMT = Y.PO.COMM.AMT
        Y.CRS.PO.VAT.AMT  = Y.PO.VAT.AMT
*--------------------VAR END--------------------------------------------
*----------------------------IF RNAU THEN BOTH TELLER & FT FOR CRS WILL BE CLEARED--------*
        Y.TT.ID = EB.SystemTables.getIdNew()
        EB.DataAccess.FRead(FN.TELLER.NAU, Y.TT.ID, REC.TT.NAU, F.TELLER.NAU, ERR.TT)
        Y.TT.REC.STATUS = REC.TT.NAU<TT.Contract.Teller.TeRecordStatus>
        Y.TT.TR.CODE = REC.TT.NAU<TT.Contract.Teller.TeTransactionCode>
*---------------------TR CODE TO RETURN---------------------------------*
        IF Y.TT.TR.CODE EQ "116" THEN
            RETURN
        END
        
*--------rev---------------------
        IF Y.TT.TR.CODE EQ "115" AND Y.TT.REC.STATUS EQ "RNAU" THEN
            Y.PO.PUR.NAME = ""
            Y.PO.COMM.AMT = ""
            Y.PO.VAT.AMT = ""
        END
    END
    
    IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' THEN
        Y.PO.PUR.NAME=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.PaymentDetails)
        Y.PO.COMM.AMT=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CommissionAmt)[4,99]
        Y.PO.VAT.AMT=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TaxAmt)[4,99]
        Y.PO.AC=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
        Y.PO.NO=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.StockNumber)
        Y.FT.TR.TYPE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TransactionType)
        
*----------------------------IF RNAU THEN BOTH TELLER & FT FOR CRS WILL BE CLEARED--------*
        Y.FT.ID = EB.SystemTables.getIdNew()
        EB.DataAccess.FRead(FN.FT.NAU, Y.FT.ID, REC.FT.NAU, F.FT.NAU, ERR.FT.NAU)
        Y.FT.REC.STATUS = REC.FT.NAU<FT.Contract.FundsTransfer.RecordStatus>
        
*---------------------TR CODE TO RETURN---------------------------------*
        IF Y.FT.TR.TYPE EQ "ACP1" THEN
            RETURN
        END
                
        IF Y.FT.TR.TYPE EQ "ACPA" AND Y.FT.REC.STATUS EQ "RNAU" THEN
            Y.PO.PUR.NAME = ""
            Y.PO.COMM.AMT = ""
            Y.PO.VAT.AMT = ""
        END
    END
    
    Y.ID='PO.':Y.PO.AC:".":Y.PO.NO
    EB.DataAccess.FRead(FN.CHQ.REG.SUP, Y.ID, Rec.CHQ.REG.SUP, F.CHQ.REG.SUP, Y.Err)
    Y.PO.CRS.DT = Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef, Y.CRS.COLL.DAT.POS>
    
*---------------------RNAU UPDATE----TT ONLY-------REV----------------------------
    IF Y.TT.TR.CODE EQ "115" AND Y.PO.CRS.DT NE "" THEN
        Y.PO.PUR.NAME = Y.CRS.PO.PUR.NAME
        Y.PO.COMM.AMT = Y.CRS.PO.COMM.AMT
        Y.PO.VAT.AMT = Y.CRS.PO.VAT.AMT
    END
*-------------------------------TT ONLY ---END-------------------------------------
  
    Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.PUR.NAME.POS>=Y.PO.PUR.NAME
    Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.COMM.AMT.POS>=Y.PO.COMM.AMT
    Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.VAT.AMT.POS>=Y.PO.VAT.AMT
    
*---------------------------WRITE CRS--------------------------------------------------
    EB.DataAccess.FWrite(FN.CHQ.REG.SUP,Y.ID,Rec.CHQ.REG.SUP)
    EB.TransactionControl.JournalUpdate(Y.ID)
    
*    WriteData = Y.PO.CRS.DT:" ":Y.FT.TR.TYPE:' Y.ID: ':Y.ID:" TR CODE ":Y.TT.TR.CODE:' Y.PO.PUR.NAME: ':Y.PO.PUR.NAME:' Y.PO.COMM.AMT: ':Y.PO.COMM.AMT:' Y.PO.VAT.AMT:':Y.PO.VAT.AMT
*    FileName = 'SHIBLI_PO_ISSUE.txt'
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

RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= GET.LOCAL.REF>
GET.LOCAL.REF:
*** <desc> </desc>
    Y.APP.NAME ="TELLER":FM:"CHEQUE.REGISTER.SUPPLEMENT"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.PUR.NAME":FM:"LT.CRS.PUR.NAME":VM:"LT.CRS.COMM.AMT":VM:"LT.CRS.VAT.AMT":VM:"LT.CRS.COLL.DAT"
    FLD.POS = ""
    EB.Updates.MultiGetLocRef(Y.APP.NAME,LOCAL.FIELDS,FLD.POS)
    Y.TT.PUR.NAME.POS=FLD.POS<1,1>
    Y.CRS.PUR.NAME.POS=FLD.POS<2,1>
    Y.CRS.COMM.AMT.POS=FLD.POS<2,2>
    Y.CRS.VAT.AMT.POS=FLD.POS<2,3>
    Y.CRS.COLL.DAT.POS = FLD.POS<2,4>
RETURN
*** </region>
END