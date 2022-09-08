* @ValidationCode : MjotMjQzODM0MzY0OkNwMTI1MjoxNjEyNDYzMDU2MDIxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Feb 2021 00:24:16
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.A.PO.COLL.CHQ.REG.SUP.WRT
*-----------------------------------------------------------------------------
**Subroutine Description:
* TNAU ROUTINE USE FOR UPDATE PO COLLECTION INFORMATION TO CHEQUE.REGISTER.SUPPLEMENT
* BASE ON GIVEN ISSUED PO ORDER NO
*Attached To    : VERSION(FUNDS.TRANSFER,MBL.PO.COLLECTION  FUNDS.TRANSFER,MBL.IW.PO.COLLECTION TELLER,MBL.PO.PAY.CASH)
*Attached As    : AUTH ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 05/11/2020 -                                NEW - MD. SAROWAR MORTOZA
* 04/02/2021 - MODIFIED ---                   MD SHIBLI MOLLAH --FDS Bangladesh Limited
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
    FN.TELLER.NAU='F.TELLER$NAU'
    F.TELLER.NAU=''
    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
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
    EB.DataAccess.Opf(FN.TELLER.NAU, F.TELLER.NAU)
    EB.DataAccess.Opf(FN.FT, F.FT)
    EB.DataAccess.Opf(FN.FT.NAU, F.FT.NAU)
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>
    GOSUB GET.LOCAL.REF ; *
    
    Y.PO.COLL.REF=EB.SystemTables.getIdNew()
    Y.PO.COLL.CO=EB.SystemTables.getIdCompany()
    
    IF EB.SystemTables.getApplication() EQ 'TELLER' THEN
        Y.PO.CANCEL=EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.PO.CANCEL.POS>
        Y.PO.COLL.DATE=EB.SystemTables.getRNew(TT.Contract.Teller.TeValueDateTwo)
        Y.PO.AC=EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)

        Y.PO.NO=EB.SystemTables.getRNew(TT.Contract.Teller.TeStockNumber)
        
*----------------------------IF RNAU THEN BOTH TELLER & FT FOR CRS WILL BE CLEARED--------*
        Y.TT.ID = EB.SystemTables.getIdNew()
        EB.DataAccess.FRead(FN.TELLER.NAU, Y.TT.ID, REC.TT.NAU, F.TELLER.NAU, ERR.TT)
        Y.TT.REC.STATUS = REC.TT.NAU<TT.Contract.Teller.TeRecordStatus>
        Y.TT.TR.CODE = REC.TT.NAU<TT.Contract.Teller.TeTransactionCode>
        
        IF Y.TT.TR.CODE EQ "115" THEN
            RETURN
        END
        
        IF Y.TT.TR.CODE EQ "116" AND Y.TT.REC.STATUS EQ "RNAU" THEN
            Y.PO.COLL.DATE = ""
            Y.PO.COLL.REF = ""
            Y.PO.COLL.CO = ""
            Y.PO.CANCEL = ""
        END
    
    END
    
    IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' THEN
        Y.PO.CANCEL=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.PO.CANCEL.POS>
        Y.PO.COLL.DATE=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitValueDate)
        Y.PO.AC=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
        Y.PO.NO=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.StockNumber)

*----------------------------IF RNAU THEN BOTH TELLER & FT FOR CRS WILL BE CLEARED--------*
        Y.FT.ID = EB.SystemTables.getIdNew()
        EB.DataAccess.FRead(FN.FT.NAU, Y.FT.ID, REC.FT.NAU, F.FT.NAU, ERR.FT.NAU)
        Y.FT.REC.STATUS = REC.FT.NAU<FT.Contract.FundsTransfer.RecordStatus>
        Y.FT.TR.TYPE = REC.FT.NAU<FT.Contract.FundsTransfer.TransactionType>
        
*---------------------TR CODE TO RETURN---------------------------------*
        IF Y.FT.TR.TYPE EQ "ACPA" THEN
            RETURN
        END
           
        IF Y.FT.TR.TYPE EQ "ACP1" AND Y.FT.REC.STATUS EQ "RNAU" THEN
            Y.PO.COLL.DATE = ""
            Y.PO.COLL.REF = ""
            Y.PO.COLL.CO = ""
            Y.PO.CANCEL = ""
        END
    END

    Y.ID='PO.':Y.PO.AC:".":Y.PO.NO
    EB.DataAccess.FRead(FN.CHQ.REG.SUP, Y.ID, Rec.CHQ.REG.SUP, F.CHQ.REG.SUP, Y.Err)
    Y.CRS.STATUS = Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsStatus>

    Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.COLL.DAT.POS>=Y.PO.COLL.DATE
    Y.PO.COLL.CHECK = Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.COLL.DAT.POS>
    Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.COLL.REF.POS>=Y.PO.COLL.REF
    Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.COLL.CO.POS>=Y.PO.COLL.CO
*----ADD NEW PO CALCEL---
    Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.PO.CANCL.POS>=Y.PO.CANCEL

    EB.DataAccess.FWrite(FN.CHQ.REG.SUP,Y.ID,Rec.CHQ.REG.SUP)
    EB.TransactionControl.JournalUpdate(Y.ID)
    
*    WriteData = Y.TT.TR.CODE:Y.FT.TR.TYPE:" PO COLL CHECK DATE ":Y.PO.COLL.CHECK:" 03RD FEB ":Y.TT.ID:" REC STATUS ":Y.TT.REC.STATUS:Y.FT.REC.STATUS:' Y.ID: ':Y.ID:' Y.PO.COLL.DATE: ':Y.PO.COLL.DATE:' Y.PO.COLL.REF:':Y.PO.COLL.REF:' Y.PO.COLL.CO:':Y.PO.COLL.CO
*    FileName = 'SHIBLI_TT.COLL.txt'
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


*--------------------ADD LT.CRS.PO.CANCL---------------------------------------------------------

*** <region name= GET.LOCAL.REF>
GET.LOCAL.REF:
*** <desc> </desc>
    Y.APP.NAME ="CHEQUE.REGISTER.SUPPLEMENT":FM:"TELLER":FM:"FUNDS.TRANSFER"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.CRS.COLL.DAT":VM:"LT.CRS.COLL.REF":VM:"LT.CRS.COLL.CO":VM:"LT.CRS.PO.CANCL":FM:"LT.TT.PO.CANCEL":FM:"LT.TT.PO.CANCEL"
    FLD.POS = ""
    EB.Updates.MultiGetLocRef(Y.APP.NAME,LOCAL.FIELDS,FLD.POS)
    Y.CRS.COLL.DAT.POS=FLD.POS<1,1>
    Y.CRS.COLL.REF.POS=FLD.POS<1,2>
    Y.CRS.COLL.CO.POS=FLD.POS<1,3>
    Y.CRS.PO.CANCL.POS = FLD.POS<1,4>
    Y.TT.PO.CANCEL.POS = FLD.POS<2,1>
    Y.FT.PO.CANCEL.POS = FLD.POS<3,1>
    
RETURN
*** </region>

END