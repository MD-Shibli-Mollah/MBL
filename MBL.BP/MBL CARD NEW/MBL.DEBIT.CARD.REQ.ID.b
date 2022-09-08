* @ValidationCode : MjotODc4MTU4Nzc3OkNwMTI1MjoxNjMyNzUwMTYyMzUwOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Sep 2021 19:42:42
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE MBL.DEBIT.CARD.REQ.ID
*-----------------------------------------------------------------------------
    !** FIELD definitions FOR MBL.DEBIT.CARD.REQ
*!
* @author smollah@fortress-global.com
* @stereotype id
* @package infra.eb
* @uses E
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING EB.SystemTables
    $USING AC.AccountOpening
    $USING EB.DataAccess
*-----------------------------------------------------------------------------
* TODO Add logic to validate the id
* TODO Create an EB.ERROR record if you are creating a new error code
*-----------------------------------------------------------------------------
    Y.ID = EB.SystemTables.getComi()
   
    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    
    EB.DataAccess.Opf(FN.AC,F.AC)
    EB.DataAccess.FRead(FN.AC,Y.ID,REC.AC,F.AC, Y.ERR)
    Y.AC.CO.CODE = REC.AC<AC.AccountOpening.Account.CoCode>
      
    IF Y.AC.CO.CODE NE EB.SystemTables.getIdCompany() THEN
        EB.SystemTables.setE('OTHER BRANCH AC')
        RETURN
    END
RETURN
END
