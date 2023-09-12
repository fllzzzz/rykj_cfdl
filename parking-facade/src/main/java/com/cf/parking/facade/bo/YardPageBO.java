package com.cf.parking.facade.bo;


import java.util.List;

import lombok.Data;
import lombok.experimental.Accessors;



/**
 * @author think
 *	停车场分页数据对象
 */
@Data
@Accessors(chain = true)
public class YardPageBO extends ParkBaseDetailRespBO {
	
	private Long total;
	
	
	private List<YardDetailBO> data;
	
	
}
