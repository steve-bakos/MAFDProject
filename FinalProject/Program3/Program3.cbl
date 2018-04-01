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
           assign to "../../../../Program2/data/S&L-records.out"
           organization is line sequential.

      *Output-file declaration
           select output-file
           assign to "../../../data/S&L Processing.out"
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
           record contains 105 characters.

       01 output-line                              pic x(105).

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
           05 ws-cnt-highest-store                 pic 99
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
           05 ws-calc-total-S-amt                  pic 9(8)v99
               value 0.
           05 ws-calc-total-L-amt                  pic 9(8)v99
               value 0.

       01 ws-report-header.
           05 filler                               pic x(42).
           05 filler                               pic x(22)
               value "SALES & LAYAWAY REPORT".
           05 filler                               pic x(9).

       01 ws-column-headers1.
           05 filler                               pic x(3).
           05 filler                               pic x(5)
               value "TRANS".
           05 filler                               pic x(5).
           05 filler                               pic x(5)
               value "TRANS".
           05 filler                               pic x(13).
           05 filler                               pic x(3)
               value "TAX".
           05 filler                               pic x(12).
           05 filler                               pic x(5)
               value "TOTAL".
           05 filler                               pic x(5).
           05 filler                               pic x(7)
               value "PAYMENT".
           05 filler                               pic x.
           05 filler                               pic x(5)
               value "STORE".
           05 filler                               pic x(5).
           05 filler                               pic x(7)
               value "INVOICE".
           05 filler                               pic x(12).
           05 filler                               pic x(3)
               value "SKU".
           
       01 ws-column-headers2.
           05 filler                               pic x(3).
           05 filler                               pic x(4)
               value "CODE".
           05 filler                               pic x(6).
           05 filler                               pic x(6)
               value "AMOUNT".
           05 filler                               pic x(11).
           05 filler                               pic x(5)
               value "OWING".
           05 filler                               pic x(11).
           05 filler                               pic x(5)
               value "OWING".
           05 filler                               pic x(7).
           05 filler                               pic x(4)
               value "TYPE".
           05 filler                               pic x(2).
           05 filler                               pic x(6)
               value "NUMBER".
           05 filler                               pic x(4).
           05 filler                               pic x(6)
               value "NUMBER".
           05 filler                               pic x(13).
           05 filler                               pic x(4)
               value "CODE".
           
       01 ws-detail-line.
           05 filler                               pic x(5).
           05 ws-dl-trans-code                     pic x.
           05 filler                               pic x(5).
           05 ws-dl-trans-amt                      pic $zzz,zz9.99.
           05 filler                               pic x(5).
           05 ws-dl-tax-owing                      pic $zzz,zz9.99.
           05 filler                               pic x(5).
           05 ws-dl-total-owing                    pic $zzz,zz9.99.
           05 filler                               pic x(5).
           05 ws-dl-pay-type                       pic xx.
           05 filler                               pic x(5).
           05 ws-dl-store-num                      pic 99.
           05 filler                               pic x(5).
           05 ws-dl-invoice                        pic x(9).
           05 filler                               pic x(5).
           05 ws-dl-sku-code                       pic x(15).

       01 ws-percentages.
           05 ws-prct-CA                           pic 9(3)
               value 0.
           05 ws-prct-CR                           pic 9(3)
               value 0.
           05 ws-prct-DB                           pic 9(3)
               value 0.

       01 ws-page-count.
           05 filler                               pic x(80).
           05 filler                               pic x(5)
               value "PAGE ".
           05 ws-pg-count                          pic 99
               value 1.

       01 ws-S-total-record-count.
           05 filler                               pic x(33)
               value "TOTAL NUMBER OF S TYPED RECORDS: ".
           05 ws-S-tot-record-count                pic zz9
               value 0.

       01 ws-L-total-record-count.
           05 filler                               pic x(33)
               value "TOTAL NUMBER OF L TYPES RECORDS: ".
           05 ws-L-tot-record-count                pic zz9
               value 0.

       01 ws-S-total-trans-amount.
           05 filler                               pic x(37)
               value "TOTAL EARNINGS FROM S TYPED RECORDS: ".
           05 ws-S-tot-trans-amt                   pic $zzz,zz9.99
               value 0.

       01 ws-L-total-trans-amount.
           05 filler                               pic x(37)
               value "TOTAL EARNINGS FROM L TYPED RECORDS: ".
           05 ws-L-tot-trans-amt                   pic $zzz,zz9.99
               value 0.

       01 ws-CA-typed-transactions.
           05 filler                               pic x(31)
               value "PERCENTAGE OF CA TRANSACTIONS: ".
           05 ws-CA-type-trans                     pic zz9
               value 0.
           05 filler                               pic x
               value "%".

       01 ws-CR-typed-transactions.
           05 filler                               pic x(31)
               value "PERCENTAGE OF CR TRANSACTIONS: ".
           05 ws-CR-type-trans                     pic zz9
               value 0.
           05 filler                               pic x
               value "%".

       01 ws-DB-typed-transactions.
           05 filler                               pic x(31)
               value "PERCENTAGE OF DB TRANSACTIONS: ".
           05 ws-DB-type-trans                     pic zz9
               value 0.
           05 filler                               pic x
               value "%".

       01 ws-total-tax-owing.
           05 filler                               pic x(17)
               value "TOTAL TAX OWING: ".
           05 ws-tot-tax                           pic $zzz,zz9.99
               value 0.

       01 ws-store-number-with-highest-SL.
           05 filler                               pic x(44)
               value "THE STORE WITH THE HIGHEST RECORD COUNT IS: ".
           05 ws-store-num-SL                      pic 99
               value 0.

       procedure division.
       000-main.

           perform 100-open-files.
           perform 200-initial-read.
           perform 300-write-report-header.
           perform 350-write-column-headers.
           perform 400-read-file until ws-eof-flag = "Y".
           perform 600-calculate-total-percentages.
           perform 700-print-totals.
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
       
      *Write the report header.
       300-write-report-header.

           write output-line from ws-report-header.
           write output-line from spaces.

      *Write the column headers for the report.
       350-write-column-headers.

           write output-line from ws-page-count.
           write output-line from spaces.
           write output-line from ws-column-headers1.
           write output-line from ws-column-headers2.
           write output-line from spaces.

      *Read through the file.
       400-read-file.

           add 1 to ws-pg-count.

           perform 450-write-detail-line
               varying ws-cnt-line-count
               from 1 by 1
               until ws-eof-flag = "Y"
                   or ws-cnt-line-count > ws-con-lines-per-page.

           write output-line from spaces.

           if (ws-eof-flag = "N") then
               write output-line from spaces
               perform 300-write-report-header
               perform 350-write-column-headers
           end-if.

      *Write the detail line.
       450-write-detail-line.

           move il-trans-code     to ws-dl-trans-code.
           move il-trans-amt      to ws-calc-trans-amt.
           move ws-calc-trans-amt to ws-dl-trans-amt.
           move il-pay-type       to ws-dl-pay-type.
           move il-store-num      to ws-dl-store-num.

           perform 500-calculations.

           move il-invoice-number to ws-dl-invoice.
           move il-sku-code       to ws-dl-sku-code.

           write output-line from ws-detail-line.

           read input-file
               at end move "Y" to ws-eof-flag.

      *Calculate tax owing and adjust counters/totals
       500-calculations.
           
           perform 510-calculate-tax-and-total-owing.
           perform 520-increment-store-counters.
           perform 530-increment-pay-type-counters.
           perform 540-increment-trans-type-counters.

      *Calculate the total tax owed on a record
       510-calculate-tax-and-total-owing.

           compute ws-calc-tax-owing   rounded
               = ( ws-calc-trans-amt   *  ws-con-tax-rate)

           move    ws-calc-tax-owing   to ws-dl-tax-owing.
           add     ws-calc-tax-owing   to ws-calc-total-tax.

      *Tax has been included here. It might not need to be.
           compute ws-calc-total-trans rounded
               = ( ws-calc-tax-owing   +  ws-calc-trans-amt).
           move    ws-calc-total-trans to ws-dl-total-owing.

           if (il-trans-code = ws-con-trans-code-L) then
               add ws-calc-total-trans to ws-calc-total-L-amt
           else
           if (il-trans-code = ws-con-trans-code-S) then
               add ws-calc-total-trans to ws-calc-total-S-amt
           end-if
           end-if.

      *Increment the approprite store counter
       520-increment-store-counters.
           if (il-store-num = ws-con-store-num-01) then
               add 1 to ws-cnt-store-01
           else
           if (il-store-num = ws-con-store-num-02) then
               add 1 to ws-cnt-store-02
           else
           if (il-store-num = ws-con-store-num-03) then
               add 1 to ws-cnt-store-03
           else
           if (il-store-num = ws-con-store-num-07) then
               add 1 to ws-cnt-store-07
           end-if
           end-if
           end-if
           end-if.

      *Increment the appropriate payment type counters
       530-increment-pay-type-counters.
           
           if (il-pay-type = ws-con-pay-type-CA) then
               add 1 to ws-cnt-pay-type-CA
           else
           if (il-pay-type = ws-con-pay-type-CR) then
               add 1 to ws-cnt-pay-type-CR
           else
           if (il-pay-type = ws-con-pay-type-DB) then
               add 1 to ws-cnt-pay-type-DB
           end-if
           end-if
           end-if.

      *Increment the transaction type counters.
       540-increment-trans-type-counters.
           if (il-trans-code = ws-con-trans-code-L) then
               add 1 to ws-cnt-L-records
           else
           if (il-trans-code = ws-con-trans-code-S) then
               add 1 to ws-cnt-S-records
           end-if
           end-if.

           add 1 to ws-cnt-total-records.

      *Calculate percentage totals
       600-calculate-total-percentages.

           compute ws-prct-CA rounded
               = (ws-cnt-pay-type-CA / ws-cnt-total-records) * 100.
           compute ws-prct-CR rounded
               = (ws-cnt-pay-type-CR / ws-cnt-total-records) * 100.

      *I'm not sure if this is the right way to handle this. The result
      *doing this is 45%, making these three total together to 100%.

      *If you round this, this number goes from 45.54 to 46, making the
      *result 101% when you add the 3 together. This lost its rounding
      *because it was the furthest away from the actual whole number
      *while the other two are much closer.
           compute ws-prct-DB 
               = (ws-cnt-pay-type-DB / ws-cnt-total-records) * 100.

      *Print the calculated totals thus far.
       700-print-totals.

           write output-line from spaces.
           move ws-cnt-L-records to ws-L-tot-record-count.
           move ws-cnt-S-records to ws-S-tot-record-count.
           write output-line from ws-L-total-record-count.
           write output-line from ws-S-total-record-count.

           write output-line from spaces.
           move ws-calc-total-L-amt to ws-L-tot-trans-amt.
           move ws-calc-total-S-amt to ws-S-tot-trans-amt.
           write output-line from ws-L-total-trans-amount.
           write output-line from ws-S-total-trans-amount.

           write output-line from spaces.
           move ws-prct-CA to ws-CA-type-trans.
           move ws-prct-CR to ws-CR-type-trans.
           move ws-prct-DB to ws-DB-type-trans.
           write output-line from ws-CA-typed-transactions.
           write output-line from ws-CR-typed-transactions.
           write output-line from ws-DB-typed-transactions.

           write output-line from spaces.
           move ws-calc-total-tax to ws-tot-tax.
           write output-line from ws-total-tax-owing.

           write output-line from spaces.

           perform 800-determine-highest-store.

           write output-line from ws-store-number-with-highest-SL.

      *Figure out which store has the highest record count
       800-determine-highest-store.

           move function max(ws-cnt-store-01, ws-cnt-store-02,
                             ws-cnt-store-03, ws-cnt-store-07)
               to ws-cnt-highest-store.

           if (ws-cnt-highest-store = ws-cnt-store-01) then
               move ws-con-store-num-01 to ws-store-num-SL
           else
           if (ws-cnt-highest-store = ws-cnt-store-02) then
               move ws-con-store-num-02 to ws-store-num-SL
           else
           if (ws-cnt-highest-store = ws-cnt-store-03) then
               move ws-con-store-num-03 to ws-store-num-SL
           else
           if (ws-cnt-highest-store = ws-cnt-store-07) then
               move ws-con-store-num-07 to ws-store-num-SL
           end-if
           end-if
           end-if
           end-if.

      *Close the used files.
       999-close-files.

           close input-file output-file.

       end program Program3.