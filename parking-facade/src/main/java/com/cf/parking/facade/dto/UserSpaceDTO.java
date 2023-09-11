package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author lpy
 * @date 2023-03-27 16:56:05
 * @description 用户车位表
 */
@Data
@Accessors(chain = true)
public class UserSpaceDTO extends PageRequest implements Serializable {


    /**
     * 工号
     */
    private String personId;

    /**
     * 姓名
     */
    private String personName;

    /**
     * 车牌号
     */
    private String plateNo;

    private List<UserSpaceValidityDTO> validity;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    private String state;

    /** 期号 */
    private Date batchNum;

    /** 摇号轮数id */
    private Long roundId;
}
