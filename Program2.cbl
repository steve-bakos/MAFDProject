       identification division.
       program-id. Program2.
       author. Sarah Powell
       date-written. 31 March 2018.
      * Description: Final Project Program 2 - Data Split and Count

       environment division.
       configuration section.
       input-output section.
       
       file-control.

      * Input-file declaration
           select input-file
               assign to "../../data/valid.out"
               organization is line sequential.

      * Output-file declarations

           select snl-file
               assign to "../../data/sales-layaway-records.out"
               organization is line sequential.

           select ret-file
               assign to "../../data/returns-records.out"
               organization is line sequential.

           select tot-file
               assign to "../../data/counts-controls-totals.out"
               organization is line sequential.


       data division.
       file section.

      * Valid data line
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 valid-in.
           05 vi-trans-code                    pic x.
           05 vi-trans-amt                     pic 9(5)v99.
           05 vi-pay-type                      pic xx.
           05 vi-store-num                     pic 99.
           05 vi-invoice-number.
               10 vi-inv-prefix                pic xx.
               10 vi-inv-hyphen                pic x.
               10 vi-inv-number                pic 9(6).
           05 vi-sku-code                      pic x(15).

      * Sales and Layaway Records out file
       fd snl-file
           data record is snl-line
           record contains 36 characters.

       01  snl-line                            pic x(36).

      * Returns out file
       fd  ret-file
           data record is rec-line
           record contains 36 characters.

       01  rec-line                            pic x(36).

      * Returns out file
       fd  tot-file
           data record is tot-line
           record contains 61 characters.

       01  tot-line                            pic x(61).

       
       working-storage section.

       01  ws-sales-layaway-line.
           05 ws-trans-code                    pic x.
           05 ws-trans-amt                     pic 9(5)v99.
           05 ws-pay-type                      pic xx.
           05 ws-store-num                     pic 99.
           05 ws-invoice-number.
               10 ws-inv-prefix                pic xx.
               10 ws-inv-hyphen                pic x.
               10 ws-inv-number                pic 9(6).
           05 ws-sku-code                      pic x(15).

       01  ws-returns-line.
           05 ws-trans-code                    pic x.
           05 ws-trans-amt                     pic 9(5)v99.
           05 ws-pay-type                      pic xx.
           05 ws-store-num                     pic 99.
           05 ws-invoice-number.
               10 ws-inv-prefix                pic xx.
               10 ws-inv-hyphen                pic x.
               10 ws-inv-number                pic 9(6).
           05 ws-sku-code                      pic x(15).
    
      * Sales and Layaway totals report lines
       01  ws-sl-header.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(24)
               value " SALES & LAYAWAY TOTALS ".
           05  filler                          pic x(23)
               value "***********************".

       01 ws-sl-transactions-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(34)
               value "TOTAL SALE & LAYAWAY TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic zz9
               value 0. 

       01 ws-sl-amount-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(27)
               value "TOTAL SALE & LAYWAY AMOUNT:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic $,$$$,$$9.99
               value 0. 

       01 ws-sl-cash-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(39)
               value "TOTAL SALE & LAYAWAY CASH TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic $,$$$,$$9.99
               value 0. 

       01 ws-sl-debit-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(40)
               value "TOTAL SALE & LAYAWAY DEBIT TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic $,$$$,$$9.99
               value 0.

       01 ws-sl-credit-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(41)
               value "TOTAL SALE & LAYAWAY CREDIT TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic $,$$$,$$9.99
               value 0.

      * Sales totals report lines

       01  ws-sales-header.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(7)
               value " SALES ".
           05  filler                          pic x(40)
               value "****************************************".

       01 ws-sales-transactions-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(24)
               value "TOTAL SALE TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic zz9
               value 0. 

       01 ws-sale-amount-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(18)
               value "TOTAL SALE AMOUNT:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic $,$$$,$$9.99
               value 0.

      * Layaway totals report lines

       01  ws-layaway-header.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(9)
               value " LAYAWAY ".
           05  filler                          pic x(38)
               value "**************************************".

       01 ws-layaway-transactions-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(27)
               value "TOTAL LAYAWAY TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic zz9
               value 0. 

       01 ws-layaway-amount-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(21)
               value "TOTAL LAYAWAY AMOUNT:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic $,$$$,$$9.99
               value 0.

      * Returns totals report lines

       01  ws-returns-header.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(9)
               value " RETURNS ".
           05  filler                          pic x(38)
               value "**************************************".

       01 ws-return-transactions-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(26)
               value "TOTAL RETURN TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic zz9
               value 0. 

       01 ws-return-amount-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(20)
               value "TOTAL RETURN AMOUNT:".
           05  filler                          pic x(2)
               value spaces.
           05  filler                          pic $,$$$,$$9.99
               value 0.

      * Sales and Layaway Transactions by Store
       01 ws-sl-store-header-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(26)
               value " SALES & LAYAWAY BY STORE ".
           05  filler                          pic x(21)
               value "*********************".

       01  ws-sl-store-transactions.
           05  filler                          pic x
               value space.
           05  filler                          pic x(45)
               value "TOTAL SALES & LAYAWAY TRANSACTIONS FOR STORE ".
           05  ws-store-number                 pic 99
               value 00.
           05  filler                          pic x
               value ":".
           05  filler                          pic x(2)
               value spaces. 
           05  filler                          pic 999
               value 0.

      * Returns Transactions by Store
       01 ws-returns-store-header-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(18)
               value " RETURNS BY STORE ".
           05  filler                          pic x(30)
               value "******************************".

       01  ws-returns-store-transactions.
           05  filler                          pic x
               value space.
           05  filler                          pic x(37)
               value "TOTAL RETURNS TRANSACTIONS FOR STORE ".
           05  ws-store-number                 pic 99
               value 00.
           05  filler                          pic x
               value ":".
           05  filler                          pic x(2)
               value spaces. 
           05  filler                          pic 999
               value 0.

             


       77  ws-eof                              pic x
               value "N".
       77  ws-sale-code                        pic x
               value "S".
       77  ws-layway-code                      pic x
               value "L".
       77  ws-return-code                      pic x
               value "R".

       procedure division.

       000-main.

           perform 100-openfiles.
           perform 200-readfiles.
           perform 300-writeline
               until ws-eof is equal to "Y".
           perform 400-writereport.
           perform 500-closefiles.

           accept return-code.
           goback.

       100-openfiles.
      * Open files
           open input input-file.
           open output snl-file ret-file tot-file.

       200-readfiles.
      * Read the record line
           read input-file
               at end move "Y" to ws-eof.

       300-writeline.
           if vi-trans-code = "S" or vi-trans-code = "L"
               write snl-line from valid-in
           else
               if vi-trans-code ="R"
                   write rec-line from valid-in
               end-if
           end-if.

      * Read next record
           read input-file at end move "Y" to ws-eof.

       400-writereport.
           write tot-line from ws-sl-header.
           write tot-line from ws-sl-transactions-line
               after advancing 1 line.
           write tot-line from ws-sl-amount-line
               after advancing 2 lines.
           write tot-line from ws-sl-cash-line
               after advancing 2 lines.
           write tot-line from ws-sl-debit-line.
           write tot-line from ws-sl-credit-line.

           write tot-line from ws-sales-header
               after advancing 1 line.
           write tot-line from ws-sales-transactions-line
               after advancing 2 lines.
           write tot-line from ws-sale-amount-line
               after advancing 2 lines.

           write tot-line from ws-layaway-header
               after advancing 2 lines.
           write tot-line from ws-layaway-transactions-line
               after advancing 2 lines.
           write tot-line from ws-layaway-amount-line
               after advancing 2 lines.

           write tot-line from ws-returns-header
               after advancing 2 lines.
           write tot-line from ws-return-transactions-line
               after advancing 2 lines.
           write tot-line from ws-return-amount-line
               after advancing 2 lines.

           write tot-line from ws-sl-store-header-line
               after advancing 2 lines.
           write tot-line from ws-sl-store-transactions
               after advancing 2 lines.

           write tot-line from ws-returns-store-header-line
               after advancing 2 lines.
           write tot-line from ws-returns-store-transactions
               after advancing 2 lines.

       500-closefiles.

           close input-file.
           close snl-file ret-file tot-file.

       end program Program2.
