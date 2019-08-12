load("311_records_summary_by_type.Rda")

## agency_table ################################################################
par(mar = c(5,3,1,1), mfrow = c(2,1))
barplot(agency_table / sum(agency_table), las = 2, ylim = c(0, 0.3))
text(20, 0.2, labels = "Most of the calls are handled by a few deptments ...")

barplot(cumsum(agency_table) / sum(agency_table), ylim = c(0,1), las = 2)
mtext(text = "... top 10 departments handles 97% of the calls.", side = 3, at = 0, adj = 0)
abline(h = 0.95, lty = 2)

## complaint_type_summary #######################################################
par(mar = c(1,3,1,1), mfrow = c(2,1))
barplot(complaint_type_summary[1:(length(complaint_type_summary)-1)] / sum(complaint_type_summary), 
        las = 2, ylim = c(0, 0.08), xaxt = 'n')
text(50, 0.07, labels = "Complaint types are a lot more dispersed ...")

barplot(cumsum(complaint_type_summary[1:(length(complaint_type_summary)-1)]) / sum(complaint_type_summary),
        ylim = c(0,1), las = 2, xaxt = 'n')
mtext(text = "... 82 types are needed to cover 95% of all calls.", side = 3, at = 0, adj = 0)
abline(h = 0.95, lty = 2)

## descriptor_summary #######################################################
par(mar = c(1,3,1,1), mfrow = c(2,1))
barplot(descriptor_summary[1:(length(descriptor_summary)-1)] / sum(descriptor_summary), 
        las = 2, ylim = c(0, 0.08), xaxt = 'n')
text(50, 0.07, labels = "Descriptors are even more dispersed ...")

barplot(cumsum(descriptor_summary[1:(length(descriptor_summary)-1)]) / sum(descriptor_summary),
        ylim = c(0,1), las = 2, xaxt = 'n')
text(x = 0, y = 0.9, labels = "... top 100 descriptors types aggregate to less than 80% of all calls.", adj = 0)
abline(h = 0.80, lty = 2)

