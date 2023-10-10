package com.cf.parking.facade.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author think
 * 申请交换车位对象
 */
@Data
@Accessors(chain = true)
public class ParkingSpaceChangeApplyDTO {

	/**
	 * 主键ID
	 */
	private Long id;
	/**
     * 申请人停车场
     */
    private String parkingCode;

    /**
     * 申请人ID
     */
    private Long userId;
    
    /**
     * 申请人工号
     */
    private String jobNumber;

    /**
     * 申请人名称
     */
    private String userName;

    /**
     * 交换人ID
     */
    private Long acceptUserId;
    
    /**
     * 交换人工号
     */
    private String acceptJobNumber;

    /**
     * 交换人名称
     */
    private String acceptUserName;

    /**
     * 交换人车库
     */
    private String acceptParkingCode;

    /**
     * 车位有效期开始日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validStartDate;

    /**
     * 车位有效期结束日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date validEndDate;

    /**
     * 状态（0：申请，1：已同意，2：已拒绝，3：已撤销）
     */
    private String state;

    


}
