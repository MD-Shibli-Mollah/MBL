* @ValidationCode : MjotMTgyODU3MjgzMjpDcDEyNTI6MTYyNTcyNDg4MjAzOTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 08 Jul 2021 12:14:42
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE CM.MBL.I.GEN.CHK
*-----------------------------------------------------------------------------
*Subroutine Description: ATTACHED TO SPECIFIC PRODUCT WHICH ARE ONLY FOR FEMALE GENDER
*Subroutine Type:
*Attached To    :  Version
*Attached As    :  ROUTINE
*-----------------------------------------------------------------------------
* Modification History 1 :
* 14/11/2019 -                            Retrofit   - MD. EBRAHIM KHALIL RIAN,
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------
* Modification History 2: RETURN is added in line number 43
*
* Date : 08 JULY 2021
* Modification Description : RETURN is added in line number 43
*
* Modified By  : MD SHIBLI MOLLAH - FDS BD
*
*------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING ST.Customer
    $USING EB.ErrorProcessing
    $USING AA.Framework
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *INITIALISATION
    GOSUB OPENFILE ; *FILE OPEN
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc>INITIALISATION </desc>
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER  = ''
    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT  = ''
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc>FILE OPEN </desc>
    EB.DataAccess.Opf(FN.CUSTOMER, F.CUSTOMER)
    EB.DataAccess.Opf(FN.AA.ARRANGEMENT, F.AA.ARRANGEMENT)
RETURN
*** </region>

*** <region name= PROCESS>
PROCESS:
*** <desc>PROCESS BUSINESS LOGIC </desc>
    Y.ARR.ID = AA.Framework.getC_aalocarrid()
    Y.AA.REC = AA.Framework.Arrangement.Read(Y.ARR.ID, E.AA)
    Y.CUS.ID = Y.AA.REC<AA.Framework.Arrangement.ArrCustomer>
    Y.ACTIVE.PRD = Y.AA.REC<AA.Framework.Arrangement.ArrActiveProduct>
    EB.DataAccess.FRead(FN.CUSTOMER, Y.CUS.ID, R.CUSTOMER, F.CUSTOMER, E.CUSTOMER)
    IF R.CUSTOMER THEN
        Y.GEN = R.CUSTOMER<ST.Customer.Customer.EbCusGender>
        IF Y.GEN NE "FEMALE" AND Y.ACTIVE.PRD EQ 'MBL.APOMBS.DP' THEN
            EB.SystemTables.setEtext("This Product not for Male Customer")
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN
*** </region>
          
END
