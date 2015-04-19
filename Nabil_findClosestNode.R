library(amap)
hc=hcluster(USArrests)
merge=hc$merge

COLUMN_1 = 1
COLUMN_2 = 2

findClosestNode<-function(dis, current_node_value){
	if(dis <= 0){
		return ()
	}
	
	current_row_column_index_pair = (which(merge==current_node_value,arr.ind = TRUE))
	current_row_index = current_row_column_index_pair[1]
	current_column_index = current_row_column_index_pair[2]

	final_result=c(current_node_value)
	if(current_node_value > 0){
		final_result = c(final_result, findClosestNode(dis-2, current_node_value))
	} 

	second_value=0
	if(current_column_index == COLUMN_2){
		second_value = merge[current_row_index, COLUMN_1]
	} else {
		second_value = merge[current_row_index, COLUMN_2]
	}

	final_result = c(final_result,second_value)
	if(second_value > 0){
		final_result = c(final_result, findClosestNode(dis-4, second_value))
	} 
	
	parent = current_row_index
	final_result = c(final_result, parent)
	final_result = c(final_result, findClosestNode(dis-3, parent))
	
	return (final_result)
}

result = findClosestNode(4, -32)
print (result)
print (unique(result))
