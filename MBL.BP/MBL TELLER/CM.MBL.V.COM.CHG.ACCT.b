* @ValidationCode : MjotMTY5ODgwNTM2MjpDcDEyNTI6MTYwOTgzODIwOTExMTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 05 Jan 2021 15:16:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CM.MBL.V.COM.CHG.ACCT
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 05TH JAN 2021
* THIS ROUTINE VALIDATES THE ACCOUNT NUMBER(CO.CODE) WITH ID.COMPANY
* FOR VERSION EB.COMPANY.CHANGE,MBL.INPUT ***
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING EB.SystemTables
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING EB.ErrorProcessing
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

*********************
INIT:
********************

    FN.ACC = "F.ACCOUNT"
    F.ACC = ""

RETURN

********************
OPENFILES:
    EB.DataAccess.Opf(FN.ACC, F.ACC)

RETURN

*****************
PROCESS:
*****************
    Y.ACC.NO = EB.SystemTables.getComi()
    Y.ID.COM = EB.SystemTables.getIdCompany()
    EB.DataAccess.FRead(FN.ACC, Y.ACC.NO, R.ACC, F.ACC, ACC.ERR)
    ACC.CO.CODE = R.ACC<AC.AccountOpening.Account.CoCode>
 
    IF ACC.CO.CODE NE Y.ID.COM THEN
        EB.SystemTables.setEtext("Enter a valid Account Number of this COMPANY")
        EB.ErrorProcessing.StoreEndError()
    END

RETURN
END