package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author lpy
 * @date 2023-03-27 09:43:43
 * @description 黑名单
 */
@Data
@Accessors(chain = true)
public class BlackListPageDTO extends PageRequest {


    /**
     * 工号
     */
    private String jobNumber;

    /**
     * 车牌号
     */
    private String plateNo;

}
