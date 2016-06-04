ds=read.csv('/Users/ps22344/Desktop/canada_final_1121.csv');
print (nrow(ds));


# tee=function(dataset, ...)
	# {
	# # print it out
	# print ("guter assi");
	# #print (str(list(as.character(substitute(...)))));
	# #print (str(list(dataset)));
	# object <- as.list(substitute(list(...)))[-1L];
	# print (length(object));
	# print (str(object));
	# do.call(aggregate, list('F1', list('ETHNICITY')), envir=dataset);
	# #with (dataset, aggregate(F1, (object), mean));
	# #with (ds, class(F1));
	# #run a func
	# }
	


my_ellipsis_function <- function(...) {
    input_list <- list(substitute(...))
    output_list <- lapply(X=input_list, function(x) {str(x);summary(x)})
    return(output_list)
}
my_ellipsis_function(a=1:10,b=11:20,c=21:30)


#aggregate(ds$F1, list(ds$G), mean)

tee=function(dataset, var)
	{
	var=eval(substitute(var), dataset);
	with(dataset, aggregate(F1, list(eval(substitute(var), dataset)), mean));
	}

tee=function(dataset, var)
	{
	var=eval(substitute(var), dataset);
	with(dataset, aggregate(F1, list(var), dataset)), mean;
	}


tee(ds, ETHNICITY)
# # Anonymous function syntax
# (function(x) x * 10)(10)
#str(ds)

