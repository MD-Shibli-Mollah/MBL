* @ValidationCode : MjotMTkyODE2MjkyMjpDcDEyNTI6MTYzNDE5MTI0NDc2MTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 14 Oct 2021 12:00:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

* Modification History 1 :
* 14/10/2021 -                     Developed By   - MD. SHIBLI MOLLAH,
*                                                 FDS Bangladesh Limited
* Added ACCOUNTS-CLOSE-ARRANGEMENT for closing arrangement from Amending Continuous Loan from Menu
* For LIVE change
*----------------------------------------------------------------------------------------------------

SUBROUTINE CR.MBL.ENQ.BLD.ACC.ACTVTY(ENQ.DATA)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.FIELDS = ENQ.DATA<2>
    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
    ENQ.DATA<2,Y.POS> = '@ID'
    ENQ.DATA<3,Y.POS> = 'EQ'
    ENQ.DATA<4,Y.POS> = 'ACCOUNTS-CHANGE.PRODUCT-ARRANGEMENT ACCOUNTS-UPDATE-BALANCE ACCOUNTS-CHANGE-DRPENALTYINT ACCOUNTS-CHANGE-DRINTEREST ACCOUNTS-UPDATE-LIMIT ACCOUNTS-CHANGE-SCHEDULE ACCOUNTS-CLOSE-ARRANGEMENT'

RETURN
END