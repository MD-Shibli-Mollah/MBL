* @ValidationCode : MjotMTgwNjY1NjQzNTpDcDEyNTI6MTYwNzU5NjQzMzYyODp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 Dec 2020 16:33:53
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.V.HEAD.TELLER.ID.COM
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 10 TH DEC 2020
* THIS ROUTINE VALIDATES THE ID(CO.CODE) WITH TELLER.ID CO.CODE
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING EB.SystemTables
    $USING TT.Contract
    $USING EB.DataAccess
    $USING EB.ErrorProcessing
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

*********************
INIT:
********************

    FN.TELLER.ID = "F.TELLER.ID"
    F.TELLER.ID = ""

RETURN

********************
OPENFILES:
    EB.DataAccess.Opf(FN.TELLER.ID, F.TELLER.ID)

RETURN

*****************
PROCESS:
*****************

    Y.CO.CODE = EB.SystemTables.getIdNew()
    Y.ID.COM = EB.SystemTables.getIdCompany()
    Y.TELLER.ID = EB.SystemTables.getComi()
    EB.DataAccess.FRead(FN.TELLER.ID, Y.TELLER.ID, R.TELLER.ID, F.TELLER.ID, TELLER.ID.ERR)
    TELLER.CO.CODE = R.TELLER.ID<TT.Contract.TellerId.TidCoCode>
 
    IF TELLER.CO.CODE NE Y.CO.CODE OR TELLER.CO.CODE NE Y.ID.COM THEN
        EB.SystemTables.setEtext("TELLER.ID IS NOT IN THIS COMPANY")
        EB.ErrorProcessing.StoreEndError()
    END
 
RETURN
END