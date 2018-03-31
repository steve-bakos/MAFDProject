       identification division.
       program-id. Program1.

       author.Steve Bakos.
       date-written.March 31st, 2018


       environment division.
       configuration section.
       input-output section.

       file-control.
      *Input-file declaration
           select input-file
           assign to "../../../data/project1.dat"
           organization is line sequential.

      *Output file declarations
           select valid-file
           assign to "../../../data/valid.out"
           organization is line sequential.

           select invalid-file
           assign to "../../../data/invalid.out"
           organization is line sequential.

       data division.
       file section.

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

       procedure division.
       000-main.

           perform 100-open-files
           perform 200-process-records until ws-eof-flag = "Y".

           perform 999-close-files
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

      *if the record is valid
       350-valid-record.

           write valid-line from input-line.

      *Close the files that were used
       999-close-files.
           close input-file invalid-file valid-file.

       end program Program1.