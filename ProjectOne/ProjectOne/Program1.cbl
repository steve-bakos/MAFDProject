       identification division.
       program-id. Program1.

       author.Steve Bakos.
       date-written.March 31st, 2018
      *Purpose: Filter out invalid records and create an error report
      *         showing what is wrong with the invalid records.

       environment division.
       configuration section.
       input-output section.

       file-control.
      *Input-file declaration
           select input-file
           assign to "../../../data/project1.dat"
           organization is line sequential.

           select error-file
           assign to "../../../data/invalid.out"
           organization is line sequential.

      *Output file declarations
           select valid-file
           assign to "../../../data/valid.out"
           organization is line sequential.

           select invalid-file
           assign to "../../../data/invalid.out"
           organization is line sequential.

           select error-report
           assign to "../../../data/error-report.out"
           organization is line sequential.

       data division.
       file section.

      *Declare the error line used to create the error report
       fd error-file
           data record is error-in
           record contains 36 characters.

       01 error-in.
           05 ei-trans-code                    pic x.
           05 ei-trans-amt                     pic 9(5)v99.
           05 ei-pay-type                      pic xx.
           05 ei-store-num                     pic 99.
           05 ei-invoice-number.
               10 ei-inv-prefix                pic xx.
               10 ei-inv-hyphen                pic x.
               10 ei-inv-number                pic 9(6).
           05 ei-sku-code                      pic x(15).

      *Declare an input line
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-trans-code                    pic x.
           05 il-trans-amt                     pic 9(5)v99.
           05 il-pay-type                      pic xx.
           05 il-store-num                     pic 99.
           05 il-invoice-number.
               10 il-inv-prefix                pic xx.
               10 il-inv-hyphen                pic x.
               10 il-inv-number                pic 9(6).
           05 il-sku-code                      pic x(15).

      *Declare a valid line
       fd valid-file
           data record is valid-line
           record contains 36 characters.

       01 valid-line                           pic x(36).

      *Declare an invalid line
       fd invalid-file
           data record is invalid-line
           record contains 36 characters.

       01 invalid-line                         pic x(36).

      *Declare an error report line
       fd error-report
           data record is error-out
           record contains 49 characters.

       01 error-out                            pic x(49).

       working-storage section.

       77 ws-eof-flag                          pic x
           value "N".

       01 ws-constants.
           05 ws-con-trns-code-S               pic x
               value "S".
           05 ws-con-trns-code-R               pic x
               value "R".
           05 ws-con-trns-code-L               pic x
               value "L".
           05 ws-con-pay-type-CA               pic xx
               value "CA".
           05 ws-con-pay-type-CR               pic xx
               value "CR".
           05 ws-con-pay-type-DB               pic xx
               value "DB".
           05 ws-con-store-num-01              pic 99
               value 01.
           05 ws-con-store-num-02              pic 99
               value 02.
           05 ws-con-store-num-03              pic 99
               value 03.
           05 ws-con-store-num-07              pic 99
               value 07.
           05 ws-con-error-star                pic x(36)
               value '************************************'.

       01 ws-counters.
           05 ws-cnt-valid-records             pic 9(4)
               value 0.
           05 ws-cnt-invalid-records           pic 9(4)
               value 0.

       01 ws-output-valid-records.
           05 ws-out-valid-total               pic x(31)
               value "Total Number of Valid Records: ".
           05 ws-out-valid-record-count        pic z(4)9
               value 0.

       01 ws-output-invalid-records.
           05 ws-out-invalid-total             pic x(32)
               value "Total Number of Invalid Records:".
           05 filler                           pic x.
           05 ws-out-invalid-record-count      pic z(2)9
               value 0.

       01 ws-error-report-header.
           05 filler                           pic x(12).
           05 filler                           pic x(12)
               value "ERROR REPORT".
           05 filler                           pic x(12).

       01 ws-error-messages.
           05 ws-ems-trns-code                 pic x(42)
               value " - This transaction code is wrong (Char 1)".
           05 ws-ems-trns-amt                  pic x(38)
               value " - Non-numeric data here (Chars 2 - 8)".
           05 ws-ems-pay-type                  pic x(40)
               value " - Incorrect payment type (Chars 9 - 10)".
           05 ws-ems-store-num                 pic x(41)
               value " - Incorrect store number (Chars 11 - 12)".
           05 ws-ems-inv-prefix                pic x(47)
               value " - Inv prefix cannot be numeric (Chars 13 - 14)".
           05 ws-ems-inv-number                pic x(49)
               value " - Inv number can only be numeric (Chars 15 - 21)"
                   .
           05 ws-ems-sku-number                pic x(47)
               value " - The SKU code cannot be blank (Chars 22 - 36)".


       01 ws-error-checks.
           05 ws-err-chk-trans-code            pic x
               value "N".
           05 ws-err-chk-trans-amt             pic x
               value "N".
           05 ws-err-chk-pay-type              pic x
               value "N".
           05 ws-err-chk-store-num             pic x
               value "N".
           05 ws-err-chk-inv-prefix            pic x
               value "N".
           05 ws-err-chk-inv-number            pic x
               value "N".
           05 ws-err-chk-sku-code              pic x
               value 'N'.

       01 ws-error-stars.
           05 ws-erstrs-trans-code             pic x.
           05 ws-erstrs-trans-amt              pic x(7).
           05 ws-erstrs-pay-type               pic xx.
           05 ws-erstrs-store-num              pic xx.
           05 ws-erstrs-inv-prefix             pic xx.
           05 filler                           pic x.
           05 ws-erstrs-inv-number             pic x(6).
           05 ws-erstrs-sku-code               pic x(15).

       procedure division.
       000-main.

           perform 100-open-files.
           perform 200-process-records until ws-eof-flag = "Y".
           perform 370-write-total-record-counts.
           perform 400-create-error-report.
           perform 999-close-files.
           goback.

      *Open the files needed
       100-open-files.

           open input input-file.
           open output valid-file invalid-file.

           read input-file
               at end move "Y" to ws-eof-flag.

      *Begin processing records
      *Start with check if the trans code is valid
       200-process-records.
           if (il-trans-code = ws-con-trns-code-S) then

               perform 210-check-trans-amount

           else
           if (il-trans-code = ws-con-trns-code-R) then

               perform 210-check-trans-amount

           else
           if (il-trans-code = ws-con-trns-code-L) then

               perform 210-check-trans-amount

           else

               perform 300-invalid-record

           end-if
           end-if
           end-if.

           read input-file
               at end move "Y" to ws-eof-flag.

      *Checking if the trans amount is a number
       210-check-trans-amount.

           if (il-trans-amt is numeric) then

               perform 220-check-payment-type

           else

               perform 300-invalid-record

           end-if.

      *Check to see if the payment type is correct
       220-check-payment-type.

           if (il-pay-type = ws-con-pay-type-CA) then

               perform 230-check-store-number

           else
           if (il-pay-type = ws-con-pay-type-CR) then

               perform 230-check-store-number

           else
           if (il-pay-type = ws-con-pay-type-DB) then

               perform 230-check-store-number

           else

               perform 300-invalid-record

           end-if
           end-if
           end-if.

      *Check to see if the store number is correct
       230-check-store-number.                 

           if (il-store-num = ws-con-store-num-01) then

                perform 240-check-invoice-prefix

           else
           if (il-store-num = ws-con-store-num-02) then

                perform 240-check-invoice-prefix

           else
           if (il-store-num = ws-con-store-num-03) then

                perform 240-check-invoice-prefix

           else
           if (il-store-num = ws-con-store-num-07) then

                perform 240-check-invoice-prefix

           else

               perform 300-invalid-record

           end-if
           end-if
           end-if
           end-if.

      *Check that the first part of the invoice is alphabetic
       240-check-invoice-prefix.

           if (il-inv-prefix is alphabetic) then

               perform 250-check-invoice-number

           else

               perform 300-invalid-record

           end-if.

      *Check that the invoice number is numeric
       250-check-invoice-number.

           if (il-inv-number is numeric) then
           
               perform 260-check-sku-number

           else

               perform 300-invalid-record

           end-if.

      *Check that the sku number isn't blank
       260-check-sku-number.

           if (il-sku-code not equal spaces) then

               perform 350-valid-record

           else

               perform 300-invalid-record

           end-if.

      *If the record is invalid
       300-invalid-record.

           write invalid-line from input-line.
           add 1 to ws-cnt-invalid-records.

      *if the record is valid
       350-valid-record.

           write valid-line from input-line.
           add 1 to ws-cnt-valid-records.

      *Write the total record counts to each file.
       370-write-total-record-counts.

           write valid-line   from spaces.
           move  ws-cnt-valid-records   to ws-out-valid-record-count.
           write valid-line   from ws-output-valid-records.

           write invalid-line from spaces.
           move  ws-cnt-invalid-records to ws-out-invalid-record-count.
           write invalid-line from ws-output-invalid-records.

      *Generate the error report
       400-create-error-report.

           close invalid-file.

           move "N" to ws-eof-flag.

           open input error-file.
           open output error-report.

           read error-file
               at end move "Y" to ws-eof-flag.

           perform 420-write-report-headers.

      *Write error report headers
       420-write-report-headers.
           write error-out from ws-error-report-header.
           write error-out from spaces.

           perform 440-write-error-records until ws-eof-flag = "Y".

      *Write the records with errors to the report
       440-write-error-records.

           write error-out from error-in.
           perform 500-list-mistakes-with-record.

           write error-out from ws-error-stars.
           move spaces to ws-error-stars.
           
           perform 460-write-error-messages.
           write error-out from spaces.

           read error-file
               at end move "Y" to ws-eof-flag.

      *Tell the user what is wrong with these records
       460-write-error-messages.
       
           if (ws-err-chk-trans-code = "Y")

               write error-out from ws-ems-trns-code
               move "N" to ws-err-chk-trans-code

           end-if.

           if (ws-err-chk-trans-amt = "Y")

               write error-out from ws-ems-trns-amt
               move "N" to ws-err-chk-trans-amt

           end-if.

           if (ws-err-chk-pay-type = "Y")

               write error-out from ws-ems-pay-type
               move "N" to ws-err-chk-pay-type

           end-if.

           if (ws-err-chk-store-num = "Y")

               write error-out from ws-ems-store-num
               move "N" to ws-err-chk-store-num

           end-if.

           if (ws-err-chk-inv-prefix = "Y")

               write error-out from ws-ems-inv-prefix
               move "N" to ws-err-chk-inv-prefix

           end-if.

           if (ws-err-chk-inv-number = "Y")

               write error-out from ws-ems-inv-number
               move "N" to ws-err-chk-inv-number

           end-if.

           if (ws-err-chk-sku-code = "Y")

               write error-out from ws-ems-sku-number
               move 'N' to ws-err-chk-sku-code

           end-if.

      *Start to list the problems with this record.
       500-list-mistakes-with-record.

           perform 510-error-trans-code.
           perform 520-error-trans-amt.
           perform 530-error-payment-type.
           perform 540-error-store-number.
           perform 550-error-invoice-prefix.
           perform 560-error-invoice-number.
           perform 570-error-sku-number.

      *Check if the transaction code is wrong
       510-error-trans-code.

           if (ei-trans-code not equal ws-con-trns-code-L  and
               ei-trans-code not equal ws-con-trns-code-R  and
               ei-trans-code not equal ws-con-trns-code-S) then

               move ws-con-error-star to ws-erstrs-trans-code
               move "Y" to ws-err-chk-trans-code

           end-if.

      *Check if the transaction amount isn't a number
       520-error-trans-amt.

           if (ei-trans-amt is not numeric) then

               move ws-con-error-star to ws-erstrs-trans-amt
               move "Y" to ws-err-chk-trans-amt

           end-if.

      *Check if the payment type is incorrect
       530-error-payment-type.

           if (ei-pay-type not equal ws-con-pay-type-CA    and
               ei-pay-type not equal ws-con-pay-type-CR    and
               ei-pay-type not equal ws-con-pay-type-DB)   then

               move ws-con-error-star to ws-erstrs-pay-type
               move "Y" to ws-err-chk-pay-type

           end-if.
       
      *Check if the store number is wrong
       540-error-store-number.

           if (ei-store-num not equal ws-con-store-num-01  and
               ei-store-num not equal ws-con-store-num-02  and
               ei-store-num not equal ws-con-store-num-03  and
               ei-store-num not equal ws-con-store-num-07) then

               move ws-con-error-star to ws-erstrs-store-num
               move "Y" to ws-err-chk-store-num

           end-if.

      *Check if the invoice prefix isn't alphabetic
       550-error-invoice-prefix.

           if (ei-inv-prefix is not alphabetic)

               move ws-con-error-star to ws-erstrs-inv-prefix
               move "Y" to ws-err-chk-inv-prefix

           end-if.

      *Check if the invoice number isn't numeric
       560-error-invoice-number.

           if (ei-inv-number is not numeric)

               move ws-con-error-star to ws-erstrs-inv-number
               move "Y" to ws-err-chk-inv-number

           end-if.

      *Check if the sku number is blank
       570-error-sku-number.

           if (ei-sku-code equals spaces)

               move ws-con-error-star to ws-erstrs-sku-code
               move "Y" to ws-err-chk-sku-code

           end-if.

      *Close the files that were used
       999-close-files.
           close input-file valid-file error-file
           error-report.
        

       end program Program1.