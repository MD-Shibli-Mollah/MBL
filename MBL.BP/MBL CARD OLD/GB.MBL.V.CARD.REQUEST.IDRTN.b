* @ValidationCode : MjotMTI3MDM3MjI3MzpDcDEyNTI6MTYyNjUyNTIzMjQyNjp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Jul 2021 18:33:52
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.V.CARD.REQUEST.IDRTN
    
* Description: This is an ID RTN to check if the ACC NUM exists or in OTHER BRANCH
*---------------------------------------------------------------------------------
* Modification History 1:
* 04/02/2021 - CREATED BY                  MD SHIBLI MOLLAH --FDS Bangladesh Limited
*-----------------------------------------------------------------------------
* Modification History 2: ID.LEN is Commented out in line number 44
*
* Date : 17th JULY 2021
* Modification Description : ID.LEN is Commented out in line number 44
*
* Modified By  : MD SHIBLI MOLLAH - FDS BD
*
*------------------------------------------------------------------------------
*---------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT T24.BP I_F.ACCOUNT
    $USING AC.AccountOpening
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing

    FN.AC='F.ACCOUNT'
    F.AC=''
    EB.DataAccess.Opf(FN.AC,F.AC)

*    Y.ID = COMI
    Y.ID = EB.SystemTables.getComi()
*ID.LEN = LEN(COMI)
*  ID.LEN = LEN(Y.ID)
*    Y.USR.COMP = ID.COMPANY
    Y.USR.COMP = EB.SystemTables.getIdCompany()

    EB.DataAccess.FRead(FN.AC,Y.ID,R.AC,F.AC,Y.ERR.AC)
* Y.AC.COMP = R.AC<AC.CO.CODE>
    Y.AC.COMP = R.AC<AC.AccountOpening.Account.CoCode>

    IF R.AC EQ '' THEN
*        E = 'WRONG ACCOUNT NUMBER'
*        CALL ERR
        EB.SystemTables.setEtext('WRONG ACCOUNT NUMBER')
        EB.ErrorProcessing.StoreEndError()
        RETURN
    END

    IF Y.USR.COMP NE Y.AC.COMP THEN
*        E = 'OTHER BRANCH AC'
*        CALL ERR
        EB.SystemTables.setEtext('OTHER BRANCH AC')
        EB.ErrorProcessing.StoreEndError()
        RETURN
    END


RETURN
END
