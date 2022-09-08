SUBROUTINE MBL.VER.TR.DISPO.FIELDS
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------
*   Author : MD. Farid Hossain
*   Last Updated : 30-06-2021
*   Last Updated by : MD. Farid Hossain
* ----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
    CALL Table.defineId("TABLE.NAME.ID", T24_String) ;* Define Table id
*-----------------------------------------------------------------------------

    CALL Table.addFieldDefinition("FIELD.NAME", "20", "": FM : "CUSTOMER.RATE_TREASURY.RATE_CREDIT.CUST.RATE_DEBIT.CUST.RATE_RATE.BOOKED","")
    CALL Table.addField("CURRENCY.MARKET", T24_String, "", "")
    CALL Table.addField("ADDITIONAL.SPREAD", T24_String, "", "")
    
*-----------------------------------------------------------------------------
*RESERVED FIELD
*-----------------------------------------------------------------------------
    CALL Table.addReservedField('RESERVED.10')
    CALL Table.addReservedField('RESERVED.09')
    CALL Table.addReservedField('RESERVED.08')
    CALL Table.addReservedField('RESERVED.07')
    CALL Table.addReservedField('RESERVED.06')
    CALL Table.addReservedField('RESERVED.05')
    CALL Table.addReservedField('RESERVED.04')
    CALL Table.addReservedField('RESERVED.03')
    CALL Table.addReservedField('RESERVED.02')
    CALL Table.addReservedField('RESERVED.01')
    
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
