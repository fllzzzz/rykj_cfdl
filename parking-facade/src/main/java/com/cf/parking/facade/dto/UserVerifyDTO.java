package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;

/**
 * 车辆审核
 * @author
 * @date 2023/9/7
 */
@Data
@Accessors(chain = true)
public class UserVerifyDTO extends PageRequest {
    /** 批量审核的ids */
    private List<Long> ids;

    /** id */
    private Long id;

    /** 申请人 */
    private String userName;

    /** 状态(0:待审核，1:审核失败,2:审核通过 3:审核不通过) */
    private String state;

    /** 申请日期（起） */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startDate;

    /** 申请日期（止） */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endDate;
}
