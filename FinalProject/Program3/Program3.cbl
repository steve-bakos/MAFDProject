       identification division.
       program-id. Program3.
       author.Steve Bakos
       date-written. April 1st, 2018
      *Purpose: Write later

       environment division.
       configuration section.
       input-output section.

       file-control.
      *Input-file declaration
           select input-file
           assign to "../../../Program2/data/sales-layaway-records.out"
           organization is line sequential.

      *Output-file declaration
           select output-file
           assign to "../../../Program3/data/S&L Processing.out"
           organization is line sequential.

       data division.
       file section.

      *Define the input file

       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-trans-code                        pic x.
           05 il-trans-amt                         pic 9(5)v99.
           05 il-pay-type                          pic xx.
           05 il-store-num                         pic 99.
           05 il-invoice-number.
               10 il-inv-prefix                    pic xx.
               10 il-inv-hyphen                    pic x.
               10 il-inv-number                    pic 9(6).
           05 il-sku-code                          pic x(15).

      *Define the output file

       fd output-file
           data record is output-line
           record contains 41 characters.

       01 output-line                              pic x(41).

       working-storage section.

       77 ws-eof-flag                              pic x
           value "N".

       01 ws-constants.
           05 ws-con-tax-rate                      pic 9v99
               value 0.13.
           05 ws-con-trans-code-S                  pic x
               value "S".
           05 ws-con-trans-code-L                  pic x
               value "L".
           05 ws-con-store-num-01                  pic 99
               value 01.
           05 ws-con-store-num-02                  pic 99
               value 02.
           05 ws-con-store-num-03                  pic 99
               value 03.
           05 ws-con-store-num-07                  pic 99
               value 07.
           05 ws-con-pay-type-CA                   pic xx
               value "CA".
           05 ws-con-pay-type-CR                   pic xx
               value "CR".
           05 ws-con-pay-type-DB                   pic xx
               value "DB".
           05 ws-con-lines-per-page                pic 99
               value 20.

       01 ws-counters.
           05 ws-cnt-S-records                     pic 9(4)
               value 0.
           05 ws-cnt-L-records                     pic 9(4)
               value 0.
           05 ws-cnt-store-01                      pic 9(4)
               value 0.
           05 ws-cnt-store-02                      pic 9(4)
               value 0.
           05 ws-cnt-store-03                      pic 9(4)
               value 0.
           05 ws-cnt-store-07                      pic 9(4)
               value 0.
           05 ws-cnt-line-count                    pic 9(4)
               value 0.
           05 ws-cnt-pay-type-CA                   pic 9(4)
               value 0.
           05 ws-cnt-pay-type-CR                   pic 9(4)
               value 0.
           05 ws-cnt-pay-type-DB                   pic 9(4)
               value 0.
           05 ws-cnt-total-records                 pic 9(4)
               value 0.

       01 ws-calculations.
           05 ws-calc-tax-owing                    pic 9(8)v99
               value 0.
           05 ws-calc-total-tax                    pic 9(8)v99
               value 0.
           05 ws-calc-trans-amt                    pic 9(8)v99
               value 0.
           05 ws-calc-total-trans                  pic 9(8)v99
               value 0.

       procedure division.
       000-main.

           perform 100-open-files.
           perform 200-initial-read.

           perform 999-close-files.


           goback.

      *Open the necessary files
       100-open-files.

           open input input-file.
           open output output-file.

      *Read the first record in the file
       200-initial-read.

           read input-file
               at end move "Y" to ws-eof-flag.
       
      *Close the used files.
       999-close-files.

           close input-file output-file.

       end program Program3.