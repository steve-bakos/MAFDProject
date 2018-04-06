       identification division.
       program-id. Program4.

       author.Steve Bakos
       date-written.April 6th, 2018
      *Purpose: Print a report based on the records that indicate
      *         an item has been returned. Shows the entire record
      *         and various totals.

       environment division.
       configuration section.
       input-output section.

       file-control.
      *Input-file declaration
           select input-file
               assign to "../../../../Program2/data/returns-records.out"
               organization is line sequential.

      *Output file declaration
           select output-file
               assign to "../../../data/returns-report.out"
               organization is line sequential.

       data division.
       file section.

      *Define the input line.
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

      *Define the output file

       fd output-file
           data record is output-line
           record contains 105 characters.

       01 output-line                              pic x(105).
       
       working-storage section.

       01 ws-report-header.
           05 filler                               pic x(30).
           05 filler                               pic x(14)
               value "RETURNS REPORT".
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
           05 filler                               pic x(5).
           05 filler                               pic x(7)
               value "PAYMENT".
           05 filler                               pic xx.
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
           05 filler                               pic x(12).
           05 filler                               pic x(4)
               value "OWED".
           05 filler                               pic x(6).
           05 filler                               pic x(4)
               value "TYPE".
           05 filler                               pic x(3).
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
           05 ws-dl-pay-type                       pic xx.
           05 filler                               pic x(5).
           05 ws-dl-store-num                      pic 99.
           05 filler                               pic x(5).
           05 ws-dl-invoice                        pic x(9).
           05 filler                               pic x(5).
           05 ws-dl-sku-code                       pic x(15).

       01 ws-R-total-record-count.
           05 filler                               pic x(33)
               value "TOTAL NUMBER OF R TYPED RECORDS: ".
           05 ws-R-tot-record-count                pic zz9
               value 0.

       01 ws-R-total-trans-amount.
           05 filler                               pic x(36)
               value "TOTAL RETURNS FROM R TYPED RECORDS: ".
           05 ws-R-tot-return-amt                  pic $zzz,zz9.99
               value 0.

       01 ws-total-tax-owing.
           05 filler                               pic x(22)
               value "TOTAL TAX OWED TO US: ".
           05 ws-tot-tax                           pic $zzz,zz9.99
               value 0.

       01 ws-constants.
           05 ws-con-tax-rate                      pic 9v99
               value 0.13.
           05 ws-con-lines-per-page                pic 99
               value 20.

       01 ws-counters.
           05 ws-cnt-R-records                     pic 9(4)
               value 0.
           05 ws-cnt-line-count                    pic 9(4)
               value 0.

       01 ws-page-count.
           05 filler                               pic x(79).
           05 filler                               pic x(5)
               value "PAGE ".
           05 ws-pg-count                          pic 99
               value 1.

       01 ws-calculations.
           05 ws-calc-total-tax                    pic 9(8)v99
               value 0.
           05 ws-calc-tax-owing                    pic 9(8)v99
               value 0.
           05 ws-calc-total-amt                    pic 9(8)v99
               value 0.
           05 ws-calc-trans-amt                    pic 9(8)v99
               value 0.
       
       01 ws-eof-flag                              pic x
           value "N".

       procedure division.
       000-main.

           perform 100-open-files.
           perform 200-initial-read.
           perform 300-write-report-header.
           perform 350-write-column-headers.
           perform 400-read-file until ws-eof-flag = "Y".

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
           else
               move ws-calc-total-tax to ws-tot-tax
               move ws-cnt-R-records  to ws-R-tot-record-count
               move ws-calc-total-amt to ws-R-tot-return-amt
               write output-line from spaces
               write output-line from ws-R-total-record-count
               write output-line from ws-R-total-trans-amount
               write output-line from ws-total-tax-owing
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
           add ws-calc-trans-amt  to ws-calc-total-amt.

           compute ws-calc-tax-owing   rounded
               = ( ws-calc-trans-amt   *  ws-con-tax-rate)

           move ws-calc-tax-owing to ws-dl-tax-owing.

           add 1 to ws-cnt-R-records.
           add ws-calc-tax-owing to ws-calc-total-tax.

      *Close the used files.
       999-close-files.

           close input-file output-file.

       end program Program4.