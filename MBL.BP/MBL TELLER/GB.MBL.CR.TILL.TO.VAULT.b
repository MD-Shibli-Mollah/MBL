* @ValidationCode : MjotMTA3NDExMTA0OTpDcDEyNTI6MTYyNTY2MTU0OTQ1Mjp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 07 Jul 2021 18:39:09
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.CR.TILL.TO.VAULT
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Description: HEAD TELLER ID SET UP for TILL.TO.VAULT
*              from EB.MBL.BRANCH.HEAD.TELLER Template
*
* Modification History :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 10 TH DEC 2020
* TELLER.ID.2
*-----------------------------------------------------------------------------
*Modification 1
*10/12/2020    TELLER,MBL.TILL.TO.VAULT
*13/1/2021 TELLER ID 2 TO 1
*-----------------------------------------------------------------------------
* Modification 2
* ADDED RETURN IN LINE 41
* Modified by MD SHIBLI MOLLAH FDS -- on 07TH JUL 2021
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING TT.Contract
    $INSERT I_F.EB.MBL.BRANCH.HEAD.TELLER
       
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *INITIALISATION
    GOSUB OPENFILE ; *FILE OPEN
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
*-----------------------------------------------------------------------------

INITIALISE:

    FN.TELLER = "F.TELLER"
    F.TELLER = ""

    FN.HEAD.TELLER = "F.EB.MBL.BRANCH.HEAD.TELLER"
    F.HEAD.TELLER = ""
RETURN

*-----------------------------------------------------------------------------

OPENFILE:
    EB.DataAccess.Opf(FN.TELLER,F.TELLER)
    EB.DataAccess.Opf(FN.HEAD.TELLER,F.HEAD.TELLER)
RETURN

PROCESS:

    Y.CO.CODE = EB.SystemTables.getIdCompany()

    EB.DataAccess.FRead(FN.HEAD.TELLER,Y.CO.CODE,R.HEAD.TELLER,F.HEAD.TELLER,HEAD.TELLER.ERR)
    Y.TELLER.ID.1 = R.HEAD.TELLER<EB.MBL70.TELLER.ID>
    
    EB.SystemTables.setRNew(TT.Contract.Teller.TeTellerIdOne, Y.TELLER.ID.1)

RETURN
END