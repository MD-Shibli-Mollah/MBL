* @ValidationCode : MjotNDIwNjQ3MjA6Q3AxMjUyOjE2MTUyNzUwNjkxNjU6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 09 Mar 2021 13:31:09
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.I.SDSA.AUTO.REF.ID
*-----------------------------------------------------------------------------
*Subroutine Description:
* THIS ROUTINE USE FOR VALIDATION OF REFERENCE NO, ACCOUNT NO
*Subroutine Type:
*Attached To    : Version (TELLER,MBL.SDSA.LCY.CASHIN TELLER,MBL.SDSA.LCY.CASHWDL TELLER,MBL.SDSA.LCY.CASHWDL.SUSP
*                         TELLER,MBL.SDSA.LCY.CASHIN.SUSP FUNDS.TRANSFER,MBL.SUSP.ORG FUNDS.TRANSFER,MBL.SUSP.ADJ FUNDS.TRANSFER,MBL.SUNDRY.ORG FUNDS.TRANSFER,MBL.SUNDRY.ADJ)
*Attached As    : INPUT ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 28/06/2020 -                            Retrofit   - Sarowar Mortoza
*                                                     FDS Pvt Ltd
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $USING ST.CompanyCreation
    $USING FT.Contract
    $USING TT.Contract
    $USING ST.Config
    $INSERT I_GTS.COMMON
    $USING EB.DataAccess
    $USING EB.Updates
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $INSERT I_F.BD.SDSA.ENTRY.DETAILS
    
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *
    GOSUB OPENFILE ; *
    GOSUB PROCESS ; *
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc> </desc>
    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''

    FN.TT = 'F.TELLER'
    F.TT = ''

    FN.CAT = 'F.CATEGORY'
    F.CAT = ''

    FN.SDSA.ENTRY = 'F.BD.SDSA.ENTRY.DETAILS'
    F.SDSA.ENTRY = ''

    FN.LOC = 'F.LOCKING'
    F.LOC = ''

    Y.FT.ORGADJ.POS = ''
    Y.FT.AL.POS = ''
    Y.TT.ORGADJ.POS = ''
    Y.TT.AL.POS = ''
    Y.CAT.AL.POS = ''
 
    Y.TODAY = EB.SystemTables.getToday()
    Y.YEAR = Y.TODAY[3,2]

    Y.APP.VER.NAME = EB.SystemTables.getApplication() : EB.SystemTables.getPgmVersion()
    
    Y.APP.NAME ="TELLER":FM:"FUNDS.TRANSFER":FM:"CATEGORY"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.ORG.ADJ":VM:"LT.A.L":VM:"LT.CIBT.SADN":FM:"LT.ORG.ADJ":VM:"LT.A.L":FM:"LT.A.L"
    FLD.POS = ""
    EB.Updates.MultiGetLocRef(Y.APP.NAME, LOCAL.FIELDS,FLD.POS)
    Y.TT.ORGADJ.POS=FLD.POS<1,1>
    Y.TT.AL.POS=FLD.POS<1,2>
    Y.TT.SADN.POS=FLD.POS<1,3>
    Y.FT.ORGADJ.POS=FLD.POS<2,1>
    Y.FT.AL.POS=FLD.POS<2,2>
    Y.CAT.AL.POS=FLD.POS<3,1>
    
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc> </desc>
    EB.DataAccess.Opf(FN.FT,F.FT)
    EB.DataAccess.Opf(FN.TT,F.TT)
    EB.DataAccess.Opf(FN.CAT,F.CAT)
    EB.DataAccess.Opf(FN.SDSA.ENTRY,F.SDSA.ENTRY)

    OPEN 'F.LOCKING' TO F.LOCKING ELSE F.LOCKING = ''
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>
    IF EB.SystemTables.getVFunction() EQ 'R' THEN
        EB.SystemTables.setEtext("Reversal of Transaction is not Allowed !!!")
        EB.ErrorProcessing.StoreEndError()
    END

    IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' THEN
        GOSUB FT.VAL.PROCESS ; *FT TXN PROCESS FOR SUNDRY SUSPENSE
    END ELSE
        IF EB.SystemTables.getApplication() EQ 'TELLER' THEN
            GOSUB TT.VAL.PROCESS ; *TT TXN PROCESS FOR SUNDRY SUSPENSE
        END
    END
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= FT.VAL.PROCESS>
FT.VAL.PROCESS:
*** <desc>FT TXN PROCESS FOR SUNDRY SUSPENSE </desc>
    Y.DR.AC.CAT=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)[4,5]
    Y.CR.AC.CAT=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)[4,5]
    EB.DataAccess.FRead(FN.CAT,Y.DR.AC.CAT,R.DR.CAT.REC,F.CAT,Y.DR.CAT.ERR)
    EB.DataAccess.FRead(FN.CAT,Y.CR.AC.CAT,R.CR.CAT.REC,F.CAT,Y.CR.CAT.ERR)
    
    IF R.DR.CAT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE '' AND R.CR.CAT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE '' THEN
        EB.SystemTables.setEtext("Both Debit/Credit Account Should not Sundry/Suspense")
        EB.ErrorProcessing.StoreEndError()
    END
    
    Y.ORGADJ.POS = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.ORGADJ.POS>
    Y.AL.POS = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS>
    BEGIN CASE
        CASE Y.ORGADJ.POS EQ 'ORG' AND Y.AL.POS EQ 'A'
            Y.AC.CAT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)[4,5]
            Y.BR.CODE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)[14,3]

            EB.DataAccess.FRead(FN.CAT,Y.AC.CAT,R.FT.REC,F.CAT,Y.FT.ERR)
            IF R.FT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE 'A' THEN
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.DebitAcctNo)
                EB.SystemTables.setEtext("Is Not Asset Account ")
                EB.ErrorProcessing.StoreEndError()
            END
            IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef) THEN
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.CreditTheirRef)
                EB.SystemTables.setEtext("Should Not be Any Value")
                EB.ErrorProcessing.StoreEndError()
            END
            IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef) NE '' THEN
                EB.DataAccess.FRead(FN.SDSA.ENTRY,EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef),R.SDSA.ENTRY.REC,F.SDSA.ENTRY,Y.SDSA.ENTRY.ERR)
            END
        CASE EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.ORGADJ.POS> EQ 'ADJ' AND EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS> EQ 'A'
            Y.AC.CAT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)[4,5]
            Y.BR.CODE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)[14,3]

            EB.DataAccess.FRead(FN.CAT,Y.AC.CAT,R.FT.REC,F.CAT,Y.FT.ERR)
            IF R.FT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE 'A' THEN
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.CreditAcctNo)
                EB.SystemTables.setEtext("Is Not Asset Account ")
                EB.ErrorProcessing.StoreEndError()
            END
            IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef) THEN
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.DebitTheirRef)
                EB.SystemTables.setEtext("Should Not be Any Value")
                EB.ErrorProcessing.StoreEndError()
            END
            IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef) NE '' THEN
                EB.DataAccess.FRead(FN.SDSA.ENTRY,EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef),R.SDSA.ENTRY.REC,F.SDSA.ENTRY,Y.SDSA.ENTRY.ERR)
                IF R.SDSA.ENTRY.REC THEN
                    IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount) GT R.SDSA.ENTRY.REC<BD.SDSA.OUTSTANDING.AMT> THEN
                        EB.SystemTables.setEtext("Amount Should Not GT Oustanding Amount")
                        EB.ErrorProcessing.StoreEndError()
                    END
                    IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo) NE R.SDSA.ENTRY.REC<BD.SDSA.AC.NUMBER> THEN
                        EB.SystemTables.setAf(FT.Contract.FundsTransfer.CreditAcctNo)
                        EB.SystemTables.setEtext("Not Belong to this Cr. Reference Number")
                        EB.ErrorProcessing.StoreEndError()
                    END
                END ELSE
                    EB.SystemTables.setEtext("Not Valid Cr. Reference Number")
                    EB.ErrorProcessing.StoreEndError()
                END
            END ELSE
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.CreditTheirRef)
                EB.SystemTables.setEtext("Should Not Null")
                EB.ErrorProcessing.StoreEndError()
            END
        CASE EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.ORGADJ.POS> EQ 'ORG' AND EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS> EQ 'L'
            Y.AC.CAT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)[4,5]
            Y.BR.CODE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)[14,3]

            EB.DataAccess.FRead(FN.CAT,Y.AC.CAT,R.FT.REC,F.CAT,Y.FT.ERR)
            IF R.FT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE 'L' THEN
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.CreditAcctNo)
                EB.SystemTables.setEtext("Is Not Liability Account ")
                EB.ErrorProcessing.StoreEndError()
            END
            IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef) THEN
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.DebitTheirRef)
                EB.SystemTables.setEtext("Should Not be Any Value")
                EB.ErrorProcessing.StoreEndError()
            END
            IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef) NE '' THEN
                EB.DataAccess.FRead(FN.SDSA.ENTRY,EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef),R.SDSA.ENTRY.REC,F.SDSA.ENTRY,Y.SDSA.ENTRY.ERR)
            END
        CASE EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.ORGADJ.POS> EQ 'ADJ' AND EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.AL.POS> EQ 'L'
            Y.AC.CAT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)[4,5]
            Y.BR.CODE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)[14,3]

            EB.DataAccess.FRead(FN.CAT,Y.AC.CAT,R.FT.REC,F.CAT,Y.FT.ERR)
            IF R.FT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE 'L' THEN
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.DebitAcctNo)
                EB.SystemTables.setEtext("Is Not Liability Account ")
                EB.ErrorProcessing.StoreEndError()
            END
            IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef) THEN
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.CreditTheirRef)
                EB.SystemTables.setEtext("Should Not be Any Value")
                EB.ErrorProcessing.StoreEndError()
            END
            IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef) NE '' THEN
                EB.DataAccess.FRead(FN.SDSA.ENTRY,EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef),R.SDSA.ENTRY.REC,F.SDSA.ENTRY,Y.SDSA.ENTRY.ERR)
                IF R.SDSA.ENTRY.REC THEN
                       
                    IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount) GT R.SDSA.ENTRY.REC<BD.SDSA.OUTSTANDING.AMT> THEN
                        EB.SystemTables.setEtext("Amount Should Not GT Oustanding Amount")
                        EB.ErrorProcessing.StoreEndError()
                    END
                    IF EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo) NE R.SDSA.ENTRY.REC<BD.SDSA.AC.NUMBER> THEN
                        EB.SystemTables.setAf(FT.Contract.FundsTransfer.DebitAcctNo)
                        EB.SystemTables.setEtext("Not Belong to this Dr. Reference Number")
                        EB.ErrorProcessing.StoreEndError()
                    END
                END ELSE
                    EB.SystemTables.setEtext("Not Valid Dr. Reference Number")
                    EB.ErrorProcessing.StoreEndError()
                END
            END ELSE
                EB.SystemTables.setAf(FT.Contract.FundsTransfer.DebitTheirRef)
                EB.SystemTables.setEtext("Should Not Null")
                EB.ErrorProcessing.StoreEndError()
            END
    END CASE

RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= TT.VAL.PROCESS>
TT.VAL.PROCESS:

*** <desc>TT TXN PROCESS FOR SUNDRY SUSPENSE </desc>
    IF EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[1,3] NE 'BDT' THEN
        EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
        EB.SystemTables.setEtext("Not Sundry/Suspense Account")
        EB.ErrorProcessing.StoreEndError()
    END

    IF Y.APP.VER.NAME EQ 'TELLER,MBL.SDSA.LCY.CASHIN' OR Y.APP.VER.NAME EQ 'TELLER,MBL.SDSA.LCY.CASHIN.SUSP' THEN

        Y.DRCR.AC.CAT=EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[4,5]
        EB.DataAccess.FRead(FN.CAT,Y.DRCR.AC.CAT,R.DRCR.CAT.REC,F.CAT,Y.DRCR.CAT.ERR)
        IF R.DRCR.CAT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> EQ '' THEN
            EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
            EB.SystemTables.setEtext("Not Sundry/Suspense Account")
            EB.ErrorProcessing.StoreEndError()

**<>
        END

        BEGIN CASE
            CASE EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.ORGADJ.POS> EQ 'ORG' AND EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS> EQ 'L'
                Y.AC.CAT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[4,5]
                Y.BR.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[14,3]
                EB.DataAccess.FRead(FN.CAT,Y.AC.CAT,R.FT.REC,F.CAT,Y.FT.ERR)
                IF R.FT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE 'L' THEN
                    EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
                    EB.SystemTables.setEtext("Is Not Liability Account ")
                    EB.ErrorProcessing.StoreEndError()
                END
                
                IF EB.SystemTables.getRNew(TT.Contract.Teller.TeOurReference) EQ '' THEN
                    IF OFS.VAL.ONLY THEN RETURN
                END ELSE
                    EB.DataAccess.FRead(FN.SDSA.ENTRY,EB.SystemTables.getRNew(TT.Contract.Teller.TeOurReference),R.SDSA.ENTRY.REC,F.SDSA.ENTRY,Y.SDSA.ENTRY.ERR)
                    IF R.SDSA.ENTRY.REC THEN
                        IF R.SDSA.ENTRY.REC<BD.SDSA.CO.CODE> NE EB.SystemTables.getIdCompany() THEN
                            EB.SystemTables.setEtext("Reference Number Not Valid Company")
                            EB.ErrorProcessing.StoreEndError()
                        END
                    END
                END

            CASE EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.ORGADJ.POS> EQ 'ADJ' AND EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS> EQ 'A'
                Y.AC.CAT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[4,5]
                Y.BR.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[14,3]
                EB.DataAccess.FRead(FN.CAT,Y.AC.CAT,R.FT.REC,F.CAT,Y.FT.ERR)
                IF R.FT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE 'A' THEN
                    EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
                    EB.SystemTables.setEtext("Is Not Asset Account ")
                    EB.ErrorProcessing.StoreEndError()
                END

                EB.DataAccess.FRead(FN.SDSA.ENTRY,EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.SADN.POS>,R.SDSA.ENTRY.REC,F.SDSA.ENTRY,Y.SDSA.ENTRY.ERR)
                IF R.SDSA.ENTRY.REC THEN
                    IF EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne) GT R.SDSA.ENTRY.REC<BD.SDSA.OUTSTANDING.AMT> THEN
                        EB.SystemTables.setEtext("Amount Should Not GT Oustanding Amount")
                        EB.ErrorProcessing.StoreEndError()
                    END
                    IF EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo) NE R.SDSA.ENTRY.REC<BD.SDSA.AC.NUMBER> THEN
                        EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
                        EB.SystemTables.setEtext("Not Belong to this Reference Number")
                        EB.ErrorProcessing.StoreEndError()
                    END
                END ELSE
                    EB.SystemTables.setEtext("Not Valid Reference Number")
                    EB.ErrorProcessing.StoreEndError()
                END
                 
        END CASE
    END

    IF Y.APP.VER.NAME EQ 'TELLER,MBL.SDSA.LCY.CASHWDL' OR Y.APP.VER.NAME EQ 'TELLER,MBL.SDSA.LCY.CASHWDL.SUSP'THEN

*Y.DRCR.AC.CAT=EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)[4,5]
        Y.DRCR.AC.CAT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[4,5]
        EB.DataAccess.FRead(FN.CAT,Y.DRCR.AC.CAT,R.DRCR.CAT.REC,F.CAT,Y.DRCR.CAT.ERR)
        IF R.DRCR.CAT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> EQ '' THEN
*EB.SystemTables.setAf(TT.Contract.Teller.TeAccountOne)
            EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
            EB.SystemTables.setEtext("Not Sundry/Suspense Account")
            EB.ErrorProcessing.StoreEndError()
        END

        BEGIN CASE
            CASE EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.ORGADJ.POS> EQ 'ORG' AND EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS> EQ 'A'
*Y.AC.CAT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)[4,5]
                Y.AC.CAT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[4,5]
*Y.BR.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)[14,3]
                Y.BR.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[14,3]
                EB.DataAccess.FRead(FN.CAT,Y.AC.CAT,R.FT.REC,F.CAT,Y.FT.ERR)
                IF R.FT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE 'A' THEN
*EB.SystemTables.setAf(TT.Contract.Teller.TeAccountOne)
                    EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
                    EB.SystemTables.setEtext("Is Not Asset Account ")
                    EB.ErrorProcessing.StoreEndError()
                END
                IF EB.SystemTables.getRNew(TT.Contract.Teller.TeOurReference) EQ '' THEN
                    IF OFS.VAL.ONLY THEN RETURN
                END ELSE
                    EB.DataAccess.FRead(FN.SDSA.ENTRY,EB.SystemTables.getRNew(TT.Contract.Teller.TeOurReference),R.SDSA.ENTRY.REC,F.SDSA.ENTRY,Y.SDSA.ENTRY.ERR)
                    IF R.SDSA.ENTRY.REC THEN
                        IF R.SDSA.ENTRY.REC<BD.SDSA.CO.CODE> NE EB.SystemTables.getIdNew() THEN
                            EB.SystemTables.setEtext("Reference Number Not Valid Company")
                            EB.ErrorProcessing.StoreEndError()
                        END
                    END
                END

            CASE EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.ORGADJ.POS> EQ 'ADJ' AND EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.AL.POS> EQ 'L'
*Y.AC.CAT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)[4,5]
                Y.AC.CAT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[4,5]
*Y.BR.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)[14,3]
                Y.BR.CODE = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)[14,3]
                EB.DataAccess.FRead(FN.CAT,Y.AC.CAT,R.FT.REC,F.CAT,Y.FT.ERR)
                IF R.FT.REC<ST.Config.Category.EbCatLocalRef,Y.CAT.AL.POS> NE 'L' THEN
*EB.SystemTables.setAf(TT.Contract.Teller.TeAccountOne)
                    EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
                    EB.SystemTables.setEtext("Is Not Liability Account ")
                    EB.ErrorProcessing.StoreEndError()
                END

                EB.DataAccess.FRead(FN.SDSA.ENTRY,EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,Y.TT.SADN.POS>,R.SDSA.ENTRY.REC,F.SDSA.ENTRY,Y.SDSA.ENTRY.ERR)

                IF R.SDSA.ENTRY.REC THEN

                    IF EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne) GT R.SDSA.ENTRY.REC<BD.SDSA.OUTSTANDING.AMT> THEN
                        EB.SystemTables.setEtext("Amount Should Not GT Oustanding Amount")
                        EB.ErrorProcessing.StoreEndError()
                    END
*IF EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne) NE R.SDSA.ENTRY.REC<BD.SDSA.AC.NUMBER> THEN
                    IF EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo) NE R.SDSA.ENTRY.REC<BD.SDSA.AC.NUMBER> THEN
*EB.SystemTables.setAf(TT.Contract.Teller.TeAccountOne)
                        EB.SystemTables.setAf(TT.Contract.Teller.TeAccountTwo)
                        EB.SystemTables.setEtext("Not Belong to this Reference Number")
                        EB.ErrorProcessing.StoreEndError()
                    END
                END ELSE
                    EB.SystemTables.setEtext("Not Valid Reference Number")
                    EB.ErrorProcessing.StoreEndError()
                END

        END CASE
    END

*    Y.VERSION = EB.SystemTables.getApplication():EB.SystemTables.getPgmVersion()
*    Y.TE.LOC.REF = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
*    Y.TE.LOC.REF<1,77> = Y.VERSION
*    EB.SystemTables.setRNew(TT.Contract.Teller.TeLocalRef, Y.TE.LOC.REF)

RETURN
*** </region>

END





