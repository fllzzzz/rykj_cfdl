package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @author lpy
 * @date 2023/3/29
 */
@Data
@Accessors(chain = true)
public class UserSpacePageDTO extends PageRequest {

    /**
     * 所属车场名(单个查询)
     */
    private String parkingLot;

    /**
     * 车牌号
     */
    private String plateNo;

    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 姓名
     */
    private String name;

    /**
     * 开始时间
     */
    private Date startDate;

    /**
     * 结束时间
     */
    private Date endDate;


}
