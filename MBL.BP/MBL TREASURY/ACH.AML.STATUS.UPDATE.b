* @ValidationCode : MjoxMTMzMTQ2NDI2OkNwMTI1MjoxNjIwODE1MDYwMDI5OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 May 2021 16:24:20
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
*-----------------------------------------------------------------------------
* <Rating>-71</Rating>
*-----------------------------------------------------------------------------
$PACKAGE ACHFRM.Foundation
SUBROUTINE ACH.AML.STATUS.UPDATE(INCOMING.ARG)
*-----------------------------------------------------------------------------
* Description   : This is the post routine attached to DFE.MAPPING to update
*                 The field AML.VERIFICATION as INITIATED, if the AML.LEVEL
*                 field value is 0
* Type          : DFE.MAPPING>POST.ROUTINE
* Linked With   : DFE.MAPPING
* In Parameter  : INCOMING.ARG
* Out Parameter : INCOMING.ARG
*-----------------------------------------------------------------------------
* Revision History:
*------------------
* Date         - 21/07/2014
* Done By      - Karthi KR
* Reference    - RTC - 1040180
* Description  - Initial Creation
*-----------------------------------------------------------------------------
* Modification History :
*  19-Mar-2016 -  Enhancement - 1562417
*                          -   Task  -  1554419
*                          -   Redesigned to update the status in ACH.BATCH and ACH.ENTRIES records
*
* 13-May-2016  - Ramarajan Chellakani
*                Defect : 1729623
*                Task   : 1729767
*                To avoid TAFC Compilation error, 2 blank lines have been included at end of the routine.
*-----------------------------------------------------------------------------
*** <region name= Inserts>
	$USING ACHFRM.Foundation
	$USING EB.SystemTables

*** </region>
*-----------------------------------------------------------------------------
*** <region name= Main Control Logic>
    GOSUB INITIALISE
    GOSUB PROCESS

    BEGIN CASE
        CASE MAPPING.ID EQ "ACH.AML.BATCH"
            GOSUB UPDATE.BATCH.STATUS
        CASE MAPPING.ID EQ "ACH.AML.ENTRIES"
            GOSUB UPDATE.ENTRIES.STATUS
    END CASE

RETURN
*** </region>

*-----------------------------------------------------------------------------
*** <region name= Initialise>
*** <desc>Common variables are initialised</desc>
INITIALISE:

    MAPPING.ID = EB.SystemTables.getBatchDetails()<3> ; ACH.AML.ID = FIELD(INCOMING.ARG,"|",1)

RETURN

*** </region>
*----------------------------------------------------------------------------------
*** <region name= PROCESS>
***<desc>Based on mapping id, Update batch or entries details</desc>
*-------
PROCESS:
*--------

    BEGIN CASE
        CASE MAPPING.ID EQ "ACH.AML.BATCH"
            GOSUB UPDATE.BATCH.STATUS
        CASE MAPPING.ID EQ "ACH.AML.ENTRIES"
            GOSUB UPDATE.ENTRIES.STATUS
    END CASE

RETURN

*-----------------------------------------------------------------------------
*** <region name= Process>
*** <desc>Read the table ACH.BATCH with the incoming value, and get the values</desc>
*** <desc>In the field AML.LEVEL, if the value is 0 then update the field</desc>
*** <desc>AML.VERIFICATION as INITIATED and write the ACH.BATCH record</desc>
*-------------------
UPDATE.BATCH.STATUS:
*-------------------

    R.ACH.BATCH = '' ; READ.ERROR = ''
    R.ACH.BATCH = ACHFRM.Foundation.AchBatch.Read(ACH.AML.ID, READ.ERROR)
    AML.LEVEL = R.ACH.BATCH<ACHFRM.Foundation.AchBatch.AchBatAmlLevels>
    IF AML.LEVEL EQ '0' THEN
        R.ACH.BATCH<ACHFRM.Foundation.AchBatch.AchBatAmlVerification> = 'INITIATED'
        ACHFRM.Foundation.AchBatch.Write(ACH.AML.ID, R.ACH.BATCH)
    END

RETURN

*** </region>
*-----------------------------------------------------------------------------
*** <region name= Process>
*** <desc>Read the table ACH.ENTRIES with the incoming value, and get the values</desc>
*** <desc>In the field AML.LEVEL, if the value is 0 then update the field</desc>
*** <desc>AML.VERIFICATION as INITIATED and write the ACH.ENTRIES record</desc>
*---------------------
UPDATE.ENTRIES.STATUS:
*---------------------

    R.ACH.ENTRIES = ACHFRM.Foundation.AchEntries.Read(ACH.AML.ID, READ.ERROR)
    IF R.ACH.ENTRIES THEN
        AML.LEVEL = R.ACH.ENTRIES<ACHFRM.Foundation.AchEntries.AchEntAmlLevels>
        IF AML.LEVEL EQ '0' THEN
            R.ACH.ENTRIES<ACHFRM.Foundation.AchEntries.AchEntAmlVerification> = 'INITIATED'
            ACHFRM.Foundation.AchEntries.Write(ACH.AML.ID, R.ACH.ENTRIES)
        END
    END

RETURN
*** </region>
END

