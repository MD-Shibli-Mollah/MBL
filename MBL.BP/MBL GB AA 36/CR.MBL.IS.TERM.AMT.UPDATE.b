* @ValidationCode : MjotMTA3NDY0OTA3MTpDcDEyNTI6MTYyNTcyNDYxNDMyNTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 08 Jul 2021 12:10:14
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

*$PACKAGE IS.ModelBank
SUBROUTINE CR.MBL.IS.TERM.AMT.UPDATE
*-----------------------------------------------------------------------------
**** <region name= Desc>
*** <desc> It describes the routine </desc>
*
* The routine IS.DEFAULT.TERM.AMOUNT defaults the AA Term amount.
* This routine is attached as a record routine to AA product of AA.PRD.DES.ACTIVITY.API
*-----------------------------------------------------------------------------
* @uses EB.SystemTables
* @uses AA.Framework
* @uses IS.Config
* @uses IS.Purchase
* @uses EB.Updates
* @uses AA.Settlement
* @package IS.ModelBank
* @class IS.DEFAULT.TERM.AMOUNT
* @stereotype subroutine
* @author rdhepikha@temenos.com

*** </region>
*-----------------------------------------------------------------------------
*** <region name= Arguments>
*** <desc>To define the arguments </desc>
* Incoming Arguments:
*
* nil
*
* Outgoing Arguments:
*
* nil
*
*** </region>
*-----------------------------------------------------------------------------
* Modifications
*
* 14/07/16 - Def  1725932
*            Task 1782401
*            Record routine to default the term amount in AA Arrangement screen.
*30/09/16 -  Def 1876903
*            Task 1877530
*            Model Bank Component used in inserts part since the model bank component
*            routines are called
*10/07/2020  S.M. SAYEED
*            Technical Consultant
*----------------------------------------------------------------------------------------
* Modification History : *$PACKAGE IS.ModelBank is commented out
*
* Date : 08 JULY 2021
* Modification Description : LINE NUM 13 is COMMENTED***
*
* Modified By  : MD SHIBLI MOLLAH - FDS BD
*-----------------------------------------------------------------------------------------

*** <region name= Inserts>

    $USING EB.SystemTables
    $USING AA.Framework
    $USING IS.Config
    $USING IS.Purchase
    $USING IS.ModelBank
    $USING EB.Updates
    $USING AA.Settlement
    $USING AA.TermAmount
    $INSERT I_AA.LOCAL.COMMON

*** </region>
*-----------------------------------------------------------------------------
    GOSUB initialise ;* Initialise the required variables
    GOSUB process ;* Main Process

RETURN
*-----------------------------------------------------------------------------

*** <region name= initialise>
initialise:
*** <desc>Initialise the required variables </desc>

    arrActivityId = ''
    RArrangement = ''

    locAppln = 'AA.ARRANGEMENT.ACTIVITY'
    locFields = 'IS.CONTRACT.REF' :@VM: 'IS.COM.SALE.REF'

    EB.Updates.MultiGetLocRef(locAppln,locFields,locPos)    ;* gets the field position of IS.CONTRACT.REF
    isContractPos = locPos<1,1>
    commSalePos = locPos<1,2>

RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= process>
process:
*** <desc>Main Process </desc>

    reqdMode = 'TRANS'

    AA.Framework.GetArrangementActivityId(reqdMode, arrActivityId)      ;* gets the AAA id
    RArrangement = AA.Framework.getRArrangementActivity()               ;* gets the AA arrangement record

    purchaseReference = RArrangement<AA.Framework.ArrangementActivity.ArrActLocalRef, isContractPos> ;* get the purchase reference
    commSaleRef = RArrangement<AA.Framework.ArrangementActivity.ArrActLocalRef, commSalePos> ;* get the Is Decl reference

    IF commSaleRef THEN
        rPurchase = IS.Purchase.Contract.Read(purchaseReference, purchaseError)
        totPurchaseAmt = rPurchase<IS.Purchase.Contract.IcTotPurchasePrice>
        EB.SystemTables.setRNew(AA.TermAmount.TermAmount.AmtChangeAmount, totPurchaseAmt) ;* to be triggered if declaration reference is provided
    END

    IF purchaseReference THEN
        GOSUB getPurchaseDetails ;* To get the required details from purchase record and make call to the corresponding routines accordingly
    END
   
RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= getPurchaseDetails>
getPurchaseDetails:
*** <desc> get the required details from purchase record and make call to the corresponding routines accordingly  </desc>

    rPurchase = ''
    purchaseError = ''
    purchaseFlag = 0
    accountingPos = ''
    prevAccountingPos = ''

    rPurchase = IS.Purchase.Contract.Read(purchaseReference, purchaseError) ;* read the purchase record

* get the required details from purchase record
    product = rPurchase<IS.Purchase.Contract.IcProduct>
    accountingEvent = rPurchase<IS.Purchase.Contract.IcAccountingEvent>
    prevAccounting = rPurchase<IS.Purchase.Contract.IcPrevAccounting>
    totPurchaseAmt = rPurchase<IS.Purchase.Contract.IcTotPurchasePrice>
    CHANGE @SM TO @VM IN prevAccounting
    
* to check whether the contract has undergone purchase accounting
    LOCATE 'PURCHASE' IN accountingEvent<1,1> SETTING accountingPos THEN
        purchaseFlag = 1
    END ELSE
        prevAccountingPos = ''
        LOCATE 'PURCHASE' IN prevAccounting<1,1> SETTING prevAccountingPos THEN
            purchaseFlag = 1
        END
    END

    IF purchaseFlag THEN
        EB.SystemTables.setRNew(AA.TermAmount.TermAmount.AmtChangeAmount, totPurchaseAmt) ;* to be triggered if contract has gone through purchase accounting
    END ELSE
        IS.ModelBank.DefIjaraTermAmt() ;* to be triggered if the contract has not been purchased
    END
    
RETURN
*** </region>END
