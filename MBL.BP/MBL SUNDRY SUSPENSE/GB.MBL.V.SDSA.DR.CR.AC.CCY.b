SUBROUTINE GB.MBL.V.SDSA.DR.CR.AC.CCY
*-----------------------------------------------------------------------------
*Subroutine Description:
*Subroutine Type:
*Attached To    : (FUNDS.TRANSFER,MBL.SUSP.ORG)
*Attached As    : VALIDATION ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 15/06/2020 -                            Retrofit   - MD.KAMRUL HASAN
*                                                 FDS Pvt Ltd
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $USING FT.Contract
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING EB.SystemTables

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

RETURN

INIT:
    
    FN.AC = 'F.ACCOUNT'
    F.AC = ''

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''

    FN.TT = 'F.TELLER'
    F.TT = ''

    FN.CAT = 'F.CATEGORY'
    F.CAT = ''

    FN.SDSA.ENTRY = 'F.BD.SDSA.ENTRY.DETAILS'
    F.SDSA.ENTRY = ''
RETURN

OPENFILES:

    EB.DataAccess.Opf(FN.AC,F.AC)

RETURN

PROCESS:

    Y.AC.ID = EB.SystemTables.getComi()

    EB.DataAccess.FRead(FN.AC,Y.AC.ID,AC.REC,F.AC,AC.ERR)
    Y.AC.CCY = AC.REC<AC.AccountOpening.Account.Currency>

    IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' THEN
        IF Y.AC.ID[1,2] NE "PL" THEN
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitCurrency,Y.AC.CCY)
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditCurrency,Y.AC.CCY)
        END ELSE
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitCurrency,"BDT")
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditCurrency,"BDT")
        END
    END

RETURN
END
