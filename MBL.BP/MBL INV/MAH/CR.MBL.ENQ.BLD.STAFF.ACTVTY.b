* @ValidationCode : MjotMTkwNjc4Njk2MDpDcDEyNTI6MTYzMTAxNjk5NjgxNzp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 07 Sep 2021 18:16:36
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

* @AUTHOR         : MD SHIBLI MOLLAH

SUBROUTINE CR.MBL.ENQ.BLD.STAFF.ACTVTY(ENQ.DATA)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :                             MD SHIBLI MOLLAH
*            BUILD ROUTINE FOR STAFF LOAN ACC        07TH SEP 2021
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.FIELDS = ENQ.DATA<2>
    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
    ENQ.DATA<2,Y.POS> = '@ID'
    ENQ.DATA<3,Y.POS> = 'EQ'
    ENQ.DATA<4,Y.POS> = 'ACCOUNTS-CHANGE.PRODUCT-ARRANGEMENT ACCOUNTS-UPDATE-ACCOUNT ACCOUNTS-CHANGE-DRPENALTYINT ACCOUNTS-CHANGE-DRINTEREST ACCOUNTS-UPDATE-LIMIT ACCOUNTS-CHANGE-SCHEDULE'

RETURN
END
END
