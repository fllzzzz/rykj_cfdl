package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Pattern;
import java.util.Date;

/**
 * @author lpy
 * @date 2023/3/29
 */
@Data
public class UserSpacePageReq extends PageRequest {
	
	@ApiModelProperty(value = "车位id")
	private Long userSpaceId;

    @ApiModelProperty(value = "所属车场名（老园区,新园区南门,地下停车场,组装车间1F,组装车间4F停车库）")
    @Pattern(regexp = "^.{0,100}$", message = "车场长度不可超过100")
    private String parkingLot;

    @ApiModelProperty(value = "车牌号")
    @Pattern(regexp = "^.{0,20}$", message = "车牌号长度不可超过20")
    private String plateNo;

    @ApiModelProperty(value = "工号")
    @Pattern(regexp = "^.{0,20}$", message = "工号长度不可超过20")
    private String jobNumber;

    @ApiModelProperty(value = "开始时间")
    private Date startDate;

    @ApiModelProperty(value = "结束时间")
    private Date endDate;

    /** 状态（0：未同步；1：同步成功；2：同步失败） */
    @ApiModelProperty(value = "状态（0：未同步；1：同步成功；2：同步失败）")
    private String state;

    /** 期号 */
    @ApiModelProperty(value = "期号")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date batchNum;

    /** 摇号轮数id */
    @ApiModelProperty(value = "摇号轮数id")
    private Long roundId;


}
