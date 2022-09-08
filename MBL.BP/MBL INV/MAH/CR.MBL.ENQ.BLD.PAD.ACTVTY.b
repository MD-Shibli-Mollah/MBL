* @ValidationCode : MjotMTMwNzEyOTYzMjpDcDEyNTI6MTYzMzUxNDYwODEwMTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Oct 2021 16:03:28
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE CR.MBL.ENQ.BLD.PAD.ACTVTY
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :                                      MD SHIBLI MOLLAH
*            BUILD ROUTINE FOR PAD LOAN ACC For LIVE          06TH OCT 2021
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